{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
module Puzzles.Akari.Problem where
import           Control.Arrow
import qualified Control.Foldl             as L
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Class
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           Data.List                 hiding (all, and, any)
import           Data.Maybe
import           Ersatz
import           Prelude                   hiding (all, and, any, not, (&&),
                                            (||))

import Puzzles.Akari.Heuristics
import Puzzles.Akari.Orphans    ()
import Puzzles.Akari.Types
import Puzzles.Classes

problem :: (MonadState s m, HasSAT s) => Configuration -> m Board
problem = problemWith <$> initialSolution <*> id

problemWith
  :: (MonadState s m, HasSAT s)
  => PartialSolution -> Configuration -> m Board
problemWith partial cfg = do
  board <- initialiseBoardWith partial cfg
  mapM_ assert $ lightInvariant cfg board
  mapM_ assert $ wallInvariant cfg board
  return board

wallInvariant
  :: (PieceLike piece, EquatableIn b piece)
  => Configuration -> Grid piece -> [b]
wallInvariant cfg@Config{..} board =
  L.foldOver (ifolded.withIndex)
    (lmap
      (\(pos, mn) -> mn <&> \n ->
        let adjs = map (both %~ map (board !)) $
                    combN (fromIntegral n) $ adjacentCells cfg pos
        in any (\(ls, rs) -> all (~== light) ls && all (~== free) rs)
            adjs
      )
    $ L.handles _Just L.list
    )
    walls

combN :: Int -> [a] -> [([a], [a])]
combN 0 xs = [([], xs)]
combN _ [] = []
combN n (x : xs) =
  map (first (x : )) (combN (n - 1) xs)
  ++ map (second (x :)) (combN n xs)

lightInvariant :: (PieceLike piece, EquatableIn b piece)
  => Configuration -> Grid piece -> [b]
lightInvariant cfg board =
  L.foldOver (ifolded . withIndex)
    (lmap
      (\(pos, pts0) -> do
        guard (isNothing $ getPlacement pos cfg)
        let conns = map (board !) $ HS.toList $ HS.delete pos pts0
        return $
              board ! pos ~== light && all (~== free) conns
          ||  board ! pos ~== free  && any (~== light) conns
      )
    $ L.handles _Just L.list
    )
  $ classifySegments cfg

validSolution
  :: Configuration
  -> PartialSolution
  -> Bool
validSolution cfg sols =
  let bd = generateGrid cfg (sols HM.!)
  in boardSize cfg == HM.size sols
    && and (lightInvariant cfg bd)
    && and (wallInvariant cfg bd)
    && and (nonWallInvariant cfg bd)

nonWallInvariant
  :: (PieceLike piece, EquatableIn b piece)
  => Configuration
  -> Grid piece
  -> [b]
nonWallInvariant cfg =
  L.foldOver
    (ifolded . withIndex)
    (lmap
      (\(pos, piece) ->
        maybe
          (piece ~== free || piece ~== light)
          ((piece ~==) . wall)
          $ HM.lookup pos $ walls cfg
      )
      L.list
    )

initialiseBoardWith
  :: (MonadState s m, HasSAT s)
  => PartialSolution -> Configuration -> m Board
initialiseBoardWith partial cfg =
  generateGridM cfg $ \pos ->
    case HM.lookup pos partial of
      Just w -> pure $ fromRawPiece w
      Nothing -> do
        v <- exists
        assert $ v === free || v === light
        pure v
