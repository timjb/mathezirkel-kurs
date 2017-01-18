{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Diagrams.BinomialCoefficients
  ( binomList
  , subsetDiagram
  , BCBackend
  , ColourScheme (..)
  , simpleColourScheme
  , bcAdditionIdentityDiagram
  , bcSymmetryDiagram
  , shiftedBcsIdentityDiagram
  , alternatingBcsIdentityDiagram
  , bcMultiplicativeIdentityDiagram
  , vandermondIdentityDiagram
  ) where

import Diagrams.Prelude
import Data.Maybe (fromJust)

type BCBackend n b =
  ( Renderable (Path V2 n) b
  , TypeableFloat n
  )

data Element
  = InSubset
  | PerDefaultInSubset
  | NotInSubset
  | Phantom
  deriving (Eq)

data ColourScheme
  = ColourScheme
  { dotColour :: Colour Double
  , selectionColour :: Colour Double
  }

type Subset = [Element]
type ColouredSubset = [(Element, ColourScheme)]

simpleColourScheme
  :: Colour Double -- ^ selection colour
  -> ColourScheme
simpleColourScheme c =
  ColourScheme
  { dotColour = black
  , selectionColour = c
  }

colourise :: ColourScheme -> Subset -> ColouredSubset
colourise scheme = flip zip (repeat scheme)

binomList :: Int -> Int -> [Subset]
binomList n k =
  case (n, k) of
    (n, 0) -> [replicate n NotInSubset]
    (0, k) -> []
    (n, k) ->
      ((InSubset :)  <$> binomList (n-1) (k-1)) ++
      ((NotInSubset :) <$> binomList (n-1) k)

subsetDiagram
  :: forall n b. BCBackend n b
  => ColouredSubset
  -> QDiagram b V2 n Any
subsetDiagram colouredSubset =
  mconcat $ zipWith renderElement [0..] colouredSubset
  where
    cardinality = fromIntegral (length colouredSubset)
    radius = cardinality * (8/5)
    renderElement :: Int -> (Element, ColourScheme) -> _
    renderElement i (element, scheme) =
      let
        el =
          case element of
            InSubset ->
              circle 3 # lwL 2 # lc (selectionColour scheme) # fc (dotColour scheme)
            NotInSubset ->
              circle 3.5 # lw 0 # fc (dotColour scheme)
            Phantom ->
              circle 3.25 # lwL 0.5 # lc gray # fc lightgray
            PerDefaultInSubset ->
              circle 3 # lwL 2 # lc (selectionColour scheme) # fc lightgray
      in
        el
          # translateY radius
          # rotateBy (- fromIntegral i / cardinality)
          # withEnvelope (circle (1.4*radius) :: D V2 n)

subsetGallery
  :: BCBackend n b
  => [ColouredSubset]
  -> QDiagram b V2 n Any
subsetGallery =
  foldr (|||) mempty .
  map (pad 1.3 . subsetDiagram)
  
sepWithRuler
  :: BCBackend n b
  => QDiagram b V2 n Any
  -> QDiagram b V2 n Any
  -> QDiagram b V2 n Any
sepWithRuler a b =
  let
    (ay1, ay2) = fromJust (extentY a)
    (by1, by2) = fromJust (extentY b)
    y1 = min ay1 by1 -- TODO: choice of min/max correct?
    y2 = max ay2 by2
    ruler =
      fromVertices [P (V2 0 y1), P (V2 0 y2)]
        -- # strokePath
        # lw 0.5
        # dashingL [1,1] 0
        # lc gray
  in
    a ||| strutX 4 ||| ruler ||| strutX 4 ||| b

-- | Proof that (n+1 choose k+1) = (n choose k) + (n choose k+1)
bcAdditionIdentityDiagram
  :: BCBackend n b
  => ColourScheme
  -> Int -- ^ n
  -> Int -- ^ k
  -> QDiagram b V2 n Any
bcAdditionIdentityDiagram scheme n k =
  subsetGallery (colourise scheme . (Phantom :) <$> binomList (n-1) k)
  ===
  subsetGallery (colourise scheme . (PerDefaultInSubset :) <$> binomList (n-1) (k-1))

-- | Proof that (n choose k) = (n choose n-k)
bcSymmetryDiagram
  :: BCBackend n b
  => ColourScheme
  -> ColourScheme
  -> Int -- ^ n
  -> Int -- ^ k
  -> QDiagram b V2 n Any
bcSymmetryDiagram primaryScheme complementaryScheme n k =
  subsetGallery (colourise primaryScheme <$> subsets)
  ===
  subsetGallery (colourise complementaryScheme . fmap complement <$> subsets)
  where
    subsets = binomList n k
    complement element =
      case element of
        InSubset -> NotInSubset
        NotInSubset -> InSubset
        PerDefaultInSubset -> Phantom
        Phantom -> PerDefaultInSubset

-- | Proof that (n choose n) + (n+1 choose n) + ... + (n+m-1 choose n) = (n+m choose n+1)
shiftedBcsIdentityDiagram
  :: BCBackend n b
  => ColourScheme
  -> Int -- ^ n
  -> Int -- ^ m
  -> QDiagram b V2 n Any
shiftedBcsIdentityDiagram colourScheme n m =
  foldr (===) mempty $
  map (subsetGallery . map (colourise colourScheme)) $ do
    i <- [0..m]
    let prefix = replicate (m - i) Phantom ++ [PerDefaultInSubset]
    [(prefix ++) <$> binomList (n+i) n]

-- | Proof that sum_i (-1)^i (n choose i) = 0
alternatingBcsIdentityDiagram
  :: BCBackend n b
  => ColourScheme
  -> Int -- ^ n
  -> QDiagram b V2 n Any
alternatingBcsIdentityDiagram colourScheme n =
  let 
    indexedBinomLists = map (\k -> (k, indexedBinomList n k)) [0..n]
    indexedBinomList n k =
      zipWith (\subset i -> (i, subset)) (binomList n k) [0..]
    subsetDiagrams =
      foldr (===) mempty $
      map (center . uncurry namedSubsetGallery) indexedBinomLists
    connections = do
      ((k, subsetsK), (l, subsetsL)) <- zip indexedBinomLists (tail indexedBinomLists)
      let
        notExtensionsK = map ((,) k . fst) $ filter (not . isExtension . snd) subsetsK
        matchingExtensionsL = map ((,) l . fst) $ filter (isExtension . snd) subsetsL
      zip notExtensionsK matchingExtensionsL
    arrowOpts =
      with
        & arrowHead .~ noHead
        & shaftStyle %~ lineColor gray
    drawConnection = connectOutside' arrowOpts
  in
    subsetDiagrams # applyAll (map (uncurry drawConnection) connections)
  where
    namedSubsetGallery k indexedSubsets =
      foldr (|||) mempty $
      map (\(j, subset) -> padY 2 $ padX 1.1 $ namedSubsetDiagram (k, j) subset) indexedSubsets
    namedSubsetDiagram :: (Int, Int) -> Subset -> _
    namedSubsetDiagram kj subset =
      (subsetDiagram (colourise colourScheme subset)
      `atop`
      (circle 15 # fc white) # lw 0) # named kj
    isExtension :: [Element] -> Bool
    isExtension xs =
      case xs of
        InSubset : InSubset : _ -> True
        NotInSubset : xs -> isExtension xs
        [InSubset] -> True -- special case
        _ -> False

-- | Proof that sum_j (m choose j) (n choose (k-j)) = (m+n choose k)
vandermondIdentityDiagram
  :: BCBackend n b
  => ColourScheme
  -> ColourScheme
  -> Int -- ^ m
  -> Int -- ^ n
  -> Int -- ^ k
  -> QDiagram b V2 n Any
vandermondIdentityDiagram schemeM schemeN m n k =
  foldr1 sepWithRuler $
  map block [0..k]
  where
    block j =
      let
        subsetsM = colourise schemeM <$> binomList m j
        subsetsN = colourise schemeN <$> binomList n (k-j)
        blockRow subsetM =
          foldr (|||) mempty $
          map (\subsetN -> subsetDiagram (subsetM ++ subsetN)) $
          subsetsN
      in
        foldr (===) mempty $
        map blockRow subsetsM

-- | Proof that k * (n choose k) = n * (n-1 choose k-1)
bcMultiplicativeIdentityDiagram
  :: BCBackend n b
  => ColourScheme -- ^ default scheme
  -> ColourScheme -- ^ marked scheme
  -> Int -- ^ n
  -> Int -- ^ k
  -> QDiagram b V2 n Any
bcMultiplicativeIdentityDiagram defaultScheme markedScheme n k =
  lhs `sepWithRuler` rhs
  where
    lhs =
      foldr (|||) mempty $
      map lhsSubsetCol (binomList n k)
    chooseOneSelected subset =
      case subset of
        [] -> []
        InSubset : xs ->
          ((InSubset, markedScheme) : fmap (, defaultScheme) xs) :
          (((InSubset, defaultScheme) :) <$> chooseOneSelected xs)
        x : xs -> ((x, defaultScheme) :) <$> chooseOneSelected xs
    lhsSubsetCol subset =
      foldr (===) mempty $
      map (pad 1.3 . subsetDiagram) (chooseOneSelected subset)
    rhsSubsets = colourise defaultScheme <$> binomList (n-1) (k-1)
    rhs =
      foldr (|||) mempty $
      map rhsCol [0..(n-1)]
    rhsCol k =
      foldr (===) mempty $
      map (pad 1.3 . subsetDiagram . insertAt k (NotInSubset, markedScheme)) rhsSubsets
    insertAt k x xs =
      take k xs ++ x : drop k xs
