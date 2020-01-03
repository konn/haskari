{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards, TupleSections #-}
module Puzzles.Akari.Heuristics where
import qualified Control.Foldl              as L
import           Control.Foldl.Extra.Vector
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.List                  as List
import           Data.Maybe
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as MV
import           Prelude
import           Puzzles.Akari.Types

type PartialSolution = HM.HashMap Position RawPiece

-- | With initial configuration and derived partial solution so far,
--   returns *newly* derived partial solutions.
--   The return solution need not extend the given one.
type Heuristics = Configuration -> PartialSolution -> PartialSolution

defaultHeuristics :: [Heuristics]
defaultHeuristics =
  [triviallyFree, maximal, onlySlot, pigeonhole3, pigeonhole]

-- | Applies heuristics successively until fixed-point.
applyHeuristics
  :: [Heuristics]
  -> Configuration
  -> PartialSolution
applyHeuristics hs cnf =
  go $ initialSolution cnf
  where
    !heuri = mconcat hs cnf
    go (!acc) =
      let new = heuri acc
      in if HM.null $ new `HM.difference` acc
      then acc
      else go (acc `HM.union` new)

initialSolution
  :: Configuration -> PartialSolution
initialSolution Config{..} = Wall <$> walls

{- | Wall 3 must have free diagonally adjacent cells.

  @
      3

  ->
    + +
     3
    + +
  @
-}
pigeonhole3 :: Heuristics
pigeonhole3 cfg =
  L.fold
    (L.handles L.folded $ lmap (,Free) L.hashMap)
  . HM.mapMaybeWithKey
    (\pos -> \case
        Wall (Just 3) ->
          Just $ diagonalCells cfg pos
        _ -> Nothing
    )

pigeonhole :: Heuristics
pigeonhole cfg@Config{..} partial =
  L.fold
    (L.handles folded
    $ lmap (,Free) L.hashMap
    )
  $ HM.mapMaybeWithKey
    (\pos -> (>>=
      (\n -> do
        let (_, lights, indets) = adjsLightsIndets cfg pos partial
            diags =
              [ d
              | d <- diagonalCells cfg pos
              , length (adjacentCells cfg d `List.intersect` indets)
                  == 2
              ]
        guard $ length indets == fromIntegral n - length lights + 1
        pure diags
      ))
    )
    walls

-- | Marks all cells as free, which is
--
--     * Lit by a light, or
--     * Adjacent empty cells, which already has
--       maximal number of adjacent lights.
triviallyFree :: Heuristics
triviallyFree cfg partial =
  let segs = classifySegments cfg
  in L.fold
      (L.handles L.folded $ lmap (,Free) L.hashMap)
    $ HM.mapMaybeWithKey
      (\pos -> \case
        Light ->
          Just $ HS.delete pos (segs ! pos)
        Wall (Just n) -> do
          let (_, lits, indets) = adjsLightsIndets cfg pos partial
          guard $ length lits == fromIntegral n
          return $ HS.fromList indets
        _ -> Nothing
      )
    partial

-- | If there is only one indeterminate cell in the segment,
--   then it must be a light.
onlySlot :: Heuristics
onlySlot cfg partial =
  let segs = classifySegments cfg
  in L.fold
      (lmap
          (\adjs -> do
            ([v], ps) <-
              pure $ List.partition (isNothing . flip HM.lookup partial)
                   $ HS.toList adjs
            guard $ all ((== Just Free).flip HM.lookup partial) ps
            return (v, Light)
          )
      $ L.handles _Just L.hashMap
      )
     $ Grid segs


-- | If there are exactly the same number of indeterminate cells
--   around a wall as remaining lights, marm them all as a light.
maximal :: Heuristics
maximal cfg partial =
  L.fold
    (L.handles L.folded $ lmap (, Light) L.hashMap)
  $ HM.mapMaybeWithKey
      (\pos ans ->
        case ans of
          Wall mn -> mn >>= \n -> do
            let (_, lits, indets) = adjsLightsIndets cfg pos partial
            guard $ length lits + length indets == fromIntegral n
            pure indets
          _ -> Nothing
      )
  partial

adjsLightsIndets
  :: Configuration
  -> Position
  -> PartialSolution
  -> ([Position], [Position], [Position])
adjsLightsIndets cfg pos partial =
  let adjs = adjacentCells cfg pos
      lits = filter ((== Just Light) . (`HM.lookup` partial)) adjs
      indets = indeterminates partial adjs
  in (adjs, lits, indets)

indeterminates
  :: Foldable t
  => PartialSolution
  -> t Position
  -> [Position]
indeterminates psol =
  filter (not . (`HM.member` psol)) . toList

{- | Two adjacent 2-blocks

@
      22
 ->
    -+  +-
    -o22o-
    -+  +-
@

TODO: implement this correctly
-}
twoTwo :: Heuristics
twoTwo = mempty

renderPartial :: Configuration -> PartialSolution -> String
renderPartial cfg partial =
  unlines $ V.toList $ V.map V.toList
  $ L.foldOver (ifolded.withIndex)
      (lmap (\(Pos{..}, a) -> (posY, (posX, a)))
      $ accumWith
          (flip $ \(i, c) ->
            V.modify (\mv -> MV.unsafeWrite mv i (renderPiece' c))
          )
        $ V.map (V.map renderPiece) (initialBoard cfg)
      )
    partial

renderPiece' :: RawPiece -> Char
renderPiece' Free = '.'
renderPiece' p    = renderPiece p
