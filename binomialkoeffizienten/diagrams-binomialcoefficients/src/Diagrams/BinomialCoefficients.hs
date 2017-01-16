{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

data Element a
  = InSubset a
  | PerDefaultInSubset a
  | NotInSubset
  | Phantom
  deriving (Functor)

binomList :: Int -> Int -> [[Element ()]]
binomList n k =
  case (n, k) of
    (n, 0) -> [replicate n NotInSubset]
    (0, k) -> []
    (n, k) ->
      ((NotInSubset :) <$> binomList (n-1) k) ++
      ((InSubset () :)  <$> binomList (n-1) (k-1))

subsetDiagram
  :: forall n b. BCBackend n b
  => [Element (Colour Double)]
  -> QDiagram b V2 n Any
subsetDiagram subset =
  mconcat $ zipWith renderElement [0..] subset
  where
    renderElement :: Int -> Element (Colour Double) -> _
    renderElement i element =
      let
        el =
          case element of
            InSubset colour ->
              circle 3 # lwL 2 # lc colour # fc black
            NotInSubset ->
              circle 3.5 # fc black
            Phantom ->
              circle 3.25 # lwL 0.5 # lc gray # fc lightgray
            PerDefaultInSubset colour ->
              circle 3 # lwL 2 # lc colour # fc lightgray
      in
        el
          # translateY 9
          # rotateBy (fromIntegral i / fromIntegral (length subset))
          # withEnvelope (circle 14 :: D V2 n)
        
subsetGallery
  :: BCBackend n b
  => [[Element (Colour Double)]]
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
  => Colour Double
  -> Int -> Int
  -> QDiagram b V2 n Any
bcAdditionFormulaDiagram colour n k =
  subsetGallery (colourise . (Phantom :) <$> binomList (n-1) k)
  `sepWithRuler`
  subsetGallery (colourise . (PerDefaultInSubset () :) <$> binomList (n-1) (k-1))
  where
    colourise = ((colour <$) <$>)

bcComplementFormulaDiagram
  :: BCBackend n b
  => Colour Double
  -> Colour Double
  -> Int -> Int
  -> QDiagram b V2 n Any
bcComplementFormulaDiagram colour complColour n k =
  subsetGallery (colourise colour <$> subsets)
  ===
  subsetGallery (colourise complColour . fmap complement <$> subsets)
  where
    subsets = binomList n k
    colourise c = ((c <$) <$>)
    complement element =
      case element of
        InSubset () -> NotInSubset
        NotInSubset -> InSubset ()
        PerDefaultInSubset () -> Phantom
        Phantom -> PerDefaultInSubset ()
        
testDia
  :: BCBackend n b
  => QDiagram b V2 n Any
testDia =
  bcAdditionFormulaDiagram teal 5 3
  ===
  bcAdditionFormulaDiagram fuchsia 6 3
  ===
  bcComplementFormulaDiagram teal fuchsia 5 3
