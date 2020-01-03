{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
module Puzzles.Akari.Problem where
import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State.Class
import qualified Data.HashSet              as HS
import           Data.List                 hiding (all, and, any)
import           Data.Maybe
import           Data.UnionFind.ST
import qualified Data.Vector               as V
import           Ersatz
import           Prelude                   hiding (all, and, any, (&&), (||))

import Puzzles.Akari.Orphans ()
import Puzzles.Akari.Types

problem
  :: (MonadState s m, HasSAT s)
  => Configuration -> m Board
problem cfg = do
  board <- initialiseBoard cfg
  lightInvariant cfg board
  wallInvariant cfg board
  return board

data Dir = Vert | Horiz
  deriving (Read, Show, Eq, Ord)

dirOffset :: Dir -> Position
dirOffset Vert  = Pos 0 1
dirOffset Horiz = Pos 1 0

dualDir :: Dir -> Dir
dualDir Vert  = Horiz
dualDir Horiz = Vert

walk :: Dir -> Position -> Position
walk  = (+) . dirOffset

classifySegments
  :: Configuration -> V.Vector (V.Vector (HS.HashSet Position))
classifySegments cfg =
   V.zipWith (V.zipWith HS.union) (go Vert) (go Horiz)
  where
    go dir = runST $ do
      bds <- generateBoardM cfg $ \pos ->
          fresh $
            if isNothing (getPlacement pos cfg)
            then HS.singleton pos
            else HS.empty
      let step pos@Pos{..} = do
            let along  = guardRange cfg $ walk dir pos
                pt = bds ! pos
            forM_ along $ \pos' -> do
              when (isNothing $ getPlacement pos cfg
                            <|> getPlacement pos' cfg
                    ) $
                  union' pt (bds ! pos') (fmap return . (<>))
              step pos'
      forM_ [0 .. maxDirSize cfg (dualDir dir) - 1] $ \i ->
        step $ fromIntegral i * dirOffset (dualDir dir)
      mapM (mapM descriptor) bds

maxDirSize :: Configuration -> Dir -> Int
maxDirSize Config{..} Vert  = boardHeight
maxDirSize Config{..} Horiz = boardWidth

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
  bd <- generateBoardM cfg (const exists)
  forBoardM_ bd $ \pos p -> assert $
    maybe (p === free || p === light)
      ((p ===) . fromRaw)
      $ getPlacement pos cfg
  return bd

adjacentCells
  :: Configuration -> Position -> [Position]
adjacentCells cfg pos =
  [ p
  | d <- [Pos (-1) 0, Pos 1 0, Pos 0 (-1), Pos 0 1]
  , let p = pos + d
  , inBoard cfg p && isNothing (getPlacement p cfg)
  ]

guardRange
  :: Configuration
  -> Position
  -> Maybe Position
guardRange cfg pos
  | inBoard cfg pos = Just pos
  | otherwise = Nothing
