{-# LANGUAGE BangPatterns, DeriveGeneric, LambdaCase, RecordWildCards #-}
{-# LANGUAGE TupleSections, ViewPatterns                              #-}
module Puzzles.Akari.Heuristics where
import           Control.Arrow              ((&&&))
import qualified Control.Foldl              as L
import           Control.Foldl.Extra.Vector
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.List                  as List
import           Data.Maybe
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as MV
import           GHC.Generics
import           Prelude
import           Puzzles.Akari.Types

type PartialSolution = HM.HashMap Position RawPiece

-- | With initial configuration and derived partial solution so far,
--   returns *newly* derived partial solutions.
--   The return solution need not extend the given one.
type Heuristics = Configuration -> Hint -> PartialSolution -> PartialSolution

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
      let hint = buildHint cnf acc
          new = heuri hint acc
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
pigeonhole3 Config{..} Hint{..} _ =
  L.foldOver (ifolded.withIndex)
    (lmap
      (\(pos, mn) -> do
        3 <- mn
        let conn = runGrid connection ! pos
        return $ diagonals conn
      )
    $ L.handles (_Just.folded)
    $ lmap (,Free) L.hashMap
    )
    walls

-- | Generalisation of 'pigeonhole3'.
--   Also applicable to walls with label other than 3.
pigeonhole :: Heuristics
pigeonhole _ Hint{..} = const $
  L.foldOver (ifolded.withIndex)
    ( lmap
        (\(pos, SlotInfo{..}) -> do
          let conn = runGrid connection ! pos
              diags =
                HS.filter
                  ( (== 2) . HS.size
                  . HS.intersection openSlots
                  . adjacents . (runGrid connection !)
                  )
                $ diagonals conn
          guard $ HS.size openSlots == remainingLights + 1
          pure diags
        )
    $ L.handles (_Just.folded)
    $ lmap (,Free) L.hashMap
    )
    slotInfo

-- | Marks all cells as free, which is
--
--     * Lit by a light, or
--     * Adjacent empty cells, which already has
--       maximal number of adjacent lights.
triviallyFree :: Heuristics
triviallyFree cfg Hint{..} partial =
  let segs = segments
  in L.fold
      (L.handles L.folded $ lmap (,Free) L.hashMap)
    $ HM.mapMaybeWithKey
      (\pos -> \case
        Light ->
          Just $ HS.delete pos (runGrid segs ! pos)
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
onlySlot _ Hint{..} partial =
  L.fold
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
      segments

-- | If there are exactly the same number of indeterminate cells
--   around a wall as remaining lights, mark them all as a light.
maximal :: Heuristics
maximal _ Hint{..} = const $
  L.foldOver (ifolded.withIndex)
    (lmap
        (\(_, SlotInfo{..}) -> do
          guard $ HS.size openSlots == remainingLights
          pure openSlots
        )
    $ L.handles (_Just . folded)
    $ lmap (,Light) L.hashMap)
    slotInfo

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
  unlines $ V.toList $ V.map fold
  $ L.foldOver (ifolded.withIndex)
      (lmap (\(Pos{..}, a) -> (posY, (posX, a)))
      $ accumWith
          (flip $ \(i, c) ->
            V.modify (\mv -> MV.unsafeWrite mv i (renderPiece' c))
          )
        $ V.map (V.map renderPiece) (initialBoard cfg)
      )
    partial

renderPiece' :: RawPiece -> String
renderPiece' Free = "."
renderPiece' p    = renderPiece p

data SlotInfo =
  SlotInfo { openSlots       :: !(HS.HashSet Position)
           , remainingLights :: !Int
           }
  deriving (Show, Eq, Ord, Generic)

data CellConnection =
  CellConn { adjacents :: !(HS.HashSet Position)
           , diagonals :: !(HS.HashSet Position)
           }
  deriving (Show, Eq, Ord, Generic)

data Hint =
  Hint { slotInfo   :: HM.HashMap Position SlotInfo
       , connection :: Grid CellConnection
       , segments   :: Grid (HS.HashSet Position)
       }
  deriving (Show, Eq, Ord, Generic)

buildHint
  :: Configuration -> PartialSolution
  -> Hint
buildHint cfg@Config{..} partial = Hint{..}
  where
    !segments = Grid $ classifySegments cfg
    !connection =
      Grid
      $ generateBoard cfg $ \pos ->
         CellConn
            { adjacents = HS.fromList $ adjacentCells cfg pos
            , diagonals = HS.fromList $ diagonalCells cfg pos
            }

    !slotInfo =
      HM.mapMaybeWithKey
        (\pos mn -> mn >>= \(fromIntegral -> n) -> do
            let adjs = adjacents
                     $ runGrid connection ! pos
                (openSlots, litCount) =
                  L.fold
                    ( lmap (id &&& (`HM.lookup` partial))
                    $ (,) <$> L.prefilter (isNothing . snd)
                                (lmap fst L.hashSet)
                          <*> L.prefilter ((== Just Light) . snd)
                                (lmap fst L.length)
                     )
                     adjs
                remainingLights = n - litCount
            return SlotInfo{..}
        )
      walls
