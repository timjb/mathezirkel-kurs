{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Diagrams.BinomialCoefficients
  ( binomList
  , subsetDiagram
  , testDia
  ) where

import Diagrams.Prelude hiding (teal, fuchsia)
import Data.Maybe (fromJust)

type BCBackend n b =
  ( Renderable (Path V2 n) b
  , TypeableFloat n
  )

-- Nice colours from http://clrs.cc/
teal, fuchsia :: (Ord a, Floating a) => Colour a
teal = sRGB24 57 204 204
fuchsia = sRGB24 240 18 190

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
      ((NotInSubset :) <$> binomList (n-1) k) ++
      ((InSubset :)  <$> binomList (n-1) (k-1))

subsetDiagram
  :: forall n b. BCBackend n b
  => ColouredSubset
  -> QDiagram b V2 n Any
subsetDiagram colouredSubset =
  mconcat $ zipWith renderElement [0..] colouredSubset
  where
    cardinality = length colouredSubset
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
          # translateY 9
          # rotateBy (- fromIntegral i / fromIntegral cardinality)
          # withEnvelope (circle 14 :: D V2 n)

subsetGallery
  :: BCBackend n b
  => [ColouredSubset]
  -> QDiagram b V2 n Any
subsetGallery =
  foldr (|||) mempty .
  map (pad 1.2 . subsetDiagram)
  
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

bcAdditionFormulaDiagram
  :: BCBackend n b
  => ColourScheme
  -> Int -- ^ n
  -> Int -- ^ k
  -> QDiagram b V2 n Any
bcAdditionFormulaDiagram scheme n k =
  subsetGallery (colourise scheme . (Phantom :) <$> binomList (n-1) k)
  `sepWithRuler`
  subsetGallery (colourise scheme . (PerDefaultInSubset :) <$> binomList (n-1) (k-1))

bcComplementFormulaDiagram
  :: BCBackend n b
  => ColourScheme
  -> ColourScheme
  -> Int -- ^ n
  -> Int -- ^ k
  -> QDiagram b V2 n Any
bcComplementFormulaDiagram primaryScheme complementaryScheme n k =
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

shiftedBcsFormulaDiagram
  :: BCBackend n b
  => Int -- ^ n
  -> Int -- ^ m
  -> QDiagram b V2 n Any
shiftedBcsFormulaDiagram n m =
  foldr (===) mempty $
  map (subsetGallery . map (colourise (simpleColourScheme teal))) $ do
    i <- [0..m]
    let prefix = replicate (m - i) Phantom ++ [PerDefaultInSubset]
    [(prefix ++) <$> binomList (n+i) n]

alternatingBcsFormulaDiagram
  :: BCBackend n b
  => Int -- ^ n
  -> QDiagram b V2 n Any
alternatingBcsFormulaDiagram n =
  --foldr (===) mempty $
  --map (subsetGallery . map colourise) $
  let 
    indexedBinomLists = map (\k -> (k, indexedBinomList n k)) [0..n]
    indexedBinomList n k =
      zipWith (\subset i -> (i, subset)) (binomList n k) [0..]
    subsetDiagrams =
      foldr (===) mempty $
      map (uncurry namedSubsetGallery) indexedBinomLists
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
        --pure (P (V2 (j*11) (k*11)), subsetDiagram subset)
    namedSubsetDiagram :: (Int, Int) -> Subset -> _
    namedSubsetDiagram kj subset =
      (subsetDiagram (colourise (simpleColourScheme teal) subset)
      `atop`
      (circle 15 # fc white) # lw 0) # named kj
    isExtension :: [Element] -> Bool
    isExtension xs =
      case xs of
        InSubset : InSubset : _ -> True
        NotInSubset : xs -> isExtension xs
        [InSubset] -> True -- special case
        _ -> False

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

testDia
  :: BCBackend n b
  => QDiagram b V2 n Any
testDia =
  bcAdditionFormulaDiagram (simpleColourScheme teal) 5 3
  ===
  bcComplementFormulaDiagram (simpleColourScheme teal) (simpleColourScheme fuchsia) 5 3
  ===
  shiftedBcsFormulaDiagram 2 3
  ===
  alternatingBcsFormulaDiagram 5
  ===
  vandermondIdentityDiagram schemeM schemeN 4 3 2
  where
    schemeM = ColourScheme { dotColour = darkgreen, selectionColour = green }
    schemeN = ColourScheme { dotColour = darkred, selectionColour = red }
    
