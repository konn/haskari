{-# LANGUAGE BangPatterns, DeriveGeneric, LambdaCase, NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns            #-}
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
  [ triviallyFree
  , maximal
  , onlySlot
  , pigeonhole
  , pigeonholeBacktrack
  , pigeonholeBacktrackDual
  ]

-- | Applies heuristics successively until fixed-point.
applyHeuristics
  :: [Heuristics]
  -> Configuration
  -> PartialSolution
applyHeuristics hs cnf =
  let sol = initialSolution cnf
  in go (buildHint cnf sol) sol
  where
    !heuri = mconcat hs cnf
    go (!hint0) (!acc) =
      let hint = updateHint cnf acc hint0
          new = heuri hint acc
      in if HM.null $ new `HM.difference` acc
      then acc
      else go hint (acc `HM.union` new)

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
        let conn = connection ! pos
        return $ HM.keys
               $ HM.filter
                  (\case { Just Wall{} -> False; _ -> True})
               $ diagonals conn
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
          let conn = connection ! pos
              diags = HM.keysSet
                $ HM.filterWithKey
                  (\pos' cell ->
                    let adjs = HM.keysSet
                              $ adjacents
                              $ connection ! pos'
                    in HS.size (openSlots `HS.intersection` adjs) == 2
                      && maybe True (not . isWall) cell
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
triviallyFree _ Hint{..} =
  L.foldOver (ifolded.withIndex)
    (lmap
      (\(pos, shape) -> case shape of
        Light ->
          Just $ HS.delete pos (segments ! pos)
        Wall Just {} -> do
          let SlotInfo{..} = slotInfo HM.! pos
          guard $ remainingLights == 0
          return openSlots
        _ -> Nothing
      )
    $ L.handles (_Just.folded)
    $ lmap (,Free) L.hashMap
    )

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
        $ V.map (V.map renderPiece) (runGrid $ initialBoard cfg)
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
  CellConn { adjacents :: !(HM.HashMap Position (Maybe RawPiece))
           , diagonals :: !(HM.HashMap Position (Maybe RawPiece))
           }
  deriving (Show, Eq, Ord, Generic)

data Hint =
  Hint { slotInfo       :: HM.HashMap Position SlotInfo
       , connection     :: Grid CellConnection
       , segments       :: Grid (HS.HashSet Position)
       , indeterminates :: HS.HashSet Position
       , allCells       :: HS.HashSet Position
       }
  deriving (Show, Eq, Ord, Generic)

updateHint
  :: Configuration -> PartialSolution
  -> Hint -> Hint
updateHint cfg@Config{..} partial
  Hint{indeterminates = indets0, segments, allCells}
  = Hint{..}
  where
    !indeterminates =
      indets0 `HS.difference` HM.keysSet partial
    !connection =
      generateGrid cfg $ \pos ->
         CellConn
            { adjacents = HM.fromList
                $ map (id &&& flip HM.lookup partial)
                $ adjacentCellsWith (const True) cfg pos
            , diagonals = HM.fromList
                $ map (id &&& flip HM.lookup partial)
                $ diagonalCellsWith (const True) cfg pos
            }

    !slotInfo =
      HM.mapMaybeWithKey
        (\pos mn -> mn >>= \(fromIntegral -> n) -> do
            let adjs = HM.keysSet
                     $ HM.filter (maybe True (not . isWall))
                     $ adjacents
                     $ connection ! pos
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

buildHint
  :: Configuration -> PartialSolution
  -> Hint
buildHint cfg@Config{..} partial = updateHint cfg partial Hint{..}
  where
    !allCells =
      HS.fromList
        [ Pos x y
        | x <- [0..boardWidth - 1]
        , y <- [0..boardHeight - 1]
        ]
      `HS.difference` HM.keysSet walls
    !indeterminates = allCells
    !segments = classifySegments cfg
    connection = Grid V.empty
    slotInfo = mempty

-- | 1-step backtracking with pigeonhole princple.
--
--   If a light cell prevents another wall from
--   having enough lights around it,
--   it must be an empty cell.
pigeonholeBacktrack :: Heuristics
pigeonholeBacktrack _ Hint{..} = const $
  L.fold
    (lmap (,Free) L.hashMap)
  $ HS.filter
    (\pos ->
      let seg = HS.delete pos $ segments ! pos
          adjWalls =
            foldMap
              ( HM.mapWithKey (const . (slotInfo HM.!))
              . HM.filter (maybe False $ \case { Wall Just{} -> True; _ -> False })
              . HM.filterWithKey (\p' _ -> p' /= pos)
              . adjacents
              . (connection !)
              )
              seg
      in any
          (\SlotInfo{..} ->
            remainingLights >
              HS.size (openSlots `HS.difference` seg)
          ) adjWalls
    )
    indeterminates

-- | 1-step backtracking with pigeonhole princple (dually).
--
--   If a numbered cell has 1 more empty slots
--   than the number of remaining light(s),
--   and if marking one of them as an empty makes
--   other wall to be unsatisfiable,
--   then it must be light
pigeonholeBacktrackDual :: Heuristics
pigeonholeBacktrackDual _ Hint{..} = const $
  L.fold
    ( L.handles folded
    $ lmap (,Light) L.hashMap
    )
  $ HM.mapMaybeWithKey
    (\pos outer -> do
      guard $ HS.size (openSlots outer) == remainingLights outer + 1
      return $ HS.filter
        (\open ->
          let remain = HS.delete open $ openSlots outer
              seg = foldMap (segments !) remain
                      `HS.difference` remain
              adjWalls =
                foldMap
                  ( HM.mapWithKey (const . (slotInfo HM.!))
                  . HM.filter (maybe False $ \case { Wall Just{} -> True; _ -> False })
                  . HM.filterWithKey (\p' _ -> p' /= pos)
                  . adjacents
                  . (connection !)
                  )
                  seg
          in any
              (\SlotInfo{..} ->
                remainingLights > HS.size (openSlots `HS.difference` seg)
                || HS.size (remain `HS.intersection` openSlots) > remainingLights
              ) adjWalls
        )
        $ openSlots outer
    )
    slotInfo

