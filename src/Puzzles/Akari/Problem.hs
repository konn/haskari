{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
module Puzzles.Akari.Problem where
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Class
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           Data.List                 hiding (all, and, any)
import           Data.Maybe
import           Ersatz
import           Prelude                   hiding (all, and, any, (&&), (||))

import Puzzles.Akari.Heuristics
import Puzzles.Akari.Orphans    ()
import Puzzles.Akari.Types

problem
  :: (MonadState s m, HasSAT s)
  => Configuration -> m Board
problem cfg = do
  board <- initialiseBoard cfg
  lightInvariant cfg board
  wallInvariant cfg board
  return board

wallInvariant
  :: (MonadState s m, HasSAT s)
  => Configuration -> Board -> m ()
wallInvariant cfg@Config{..} board = iforM_ walls $ \pos mcount ->
  forM_ mcount $ \n -> do
    let adjs = map (both %~ map (board !)) $
               combN (fromIntegral n) $ adjacentCells cfg pos
    assert $
      any (\(ls, rs) -> all (=== light) ls && all (=== free) rs)
      adjs

combN :: Int -> [a] -> [([a], [a])]
combN 0 xs = [([], xs)]
combN _ [] = []
combN n (x : xs) =
  map (first (x : )) (combN (n - 1) xs)
  ++ map (second (x :)) (combN n xs)

lightInvariant :: (MonadState s m, HasSAT s)
  => Configuration -> Board -> m ()
lightInvariant cfg board =
  forBoardM_ (classifySegments cfg) $ \pos pts0 ->
    when (isNothing $ getPlacement pos cfg) $
    let conns = HS.toList $ HS.delete pos pts0
    in assert $
          board ! pos === light &&
            all ((=== free) . (board !))
            conns
          ||    board ! pos === free
             && any ((=== light) . (board !)) conns

initialiseBoard
  :: (MonadState s m, HasSAT s)
  => Configuration -> m Board
initialiseBoard cfg = do
  let partial = applyHeuristics defaultHeuristics cfg
  generateBoardM cfg $ \pos ->
    case HM.lookup pos partial of
      Just w -> pure $ fromRaw w
      Nothing -> do
        v <- exists
        assert $ v === free || v === light
        pure v
