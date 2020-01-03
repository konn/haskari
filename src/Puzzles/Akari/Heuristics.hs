{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards, TupleSections #-}
module Puzzles.Akari.Heuristics where
import qualified Control.Foldl       as L
import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.Profunctor
import           Prelude
import           Puzzles.Akari.Types

type PartialSolution = HM.HashMap Position RawPiece

-- | With initial configuration and derived partial solution so far,
--   returns *newly* derived partial solutions.
--   The return solution need not extend the given one.
type Heuristics = Configuration -> PartialSolution -> PartialSolution

defaultHeuristics :: [Heuristics]
defaultHeuristics = [triviallyFree, maximal, pigeonhole3]

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

triviallyFree :: Heuristics
triviallyFree cfg =
  let segs = classifySegments cfg
  in L.fold
      (L.handles L.folded $ lmap (,Free) L.hashMap)
    . HM.mapMaybeWithKey
    (\pos -> \case
      Light ->
        Just $ HS.delete pos (segs ! pos)
      _ -> Nothing
    )

maximal :: Heuristics
maximal cfg partial =
  L.fold
    (L.handles L.folded $ lmap (, Light) L.hashMap)
  $ HM.mapMaybeWithKey
      (\pos ans ->
        case ans of
          Wall mn -> mn >>= \n -> do
            let adjs = adjacentCells cfg pos
                lits = filter ((== Just Light) . (`HM.lookup` partial)) adjs
                indets = indeterminates partial adjs
            guard $ length lits + length indets == fromIntegral n
            pure adjs
          _ -> Nothing
      )
  partial

indeterminates
  :: Foldable t
  => PartialSolution
  -> t Position
  -> [Position]
indeterminates psol =
  filter (not . (`HM.member` psol))
  . toList

