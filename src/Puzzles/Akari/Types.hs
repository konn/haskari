{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric            #-}
{-# LANGUAGE DeriveTraversable, DerivingStrategies, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms, RecordWildCards, TypeFamilies               #-}
module Puzzles.Akari.Types where
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.Maybe
import           Data.UnionFind.ST
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word
import           Ersatz              hiding ((&&), (||))
import           GHC.Generics
import           Type.Reflection

data Configuration =
  Config { boardHeight :: !Int
         , boardWidth  :: !Int
         , walls       :: Walls
         }
  deriving (Show, Eq, Ord, Generic, Typeable)

type WallSpec = Maybe Word8

fromRawWall :: WallSpec -> Piece
fromRawWall = wall

fromRaw :: RawPiece -> Piece
fromRaw (Wall w) = wall w
fromRaw Light    = light
fromRaw Free     = free

type Walls = HashMap Position WallSpec

(!) :: Vector (Vector a) -> Position -> a
(!) b (Pos x y) = b V.! y V.! x

(!?) :: Vector (Vector a) -> Position -> Maybe a
b !? Pos x y = (b V.!? y) >>= (V.!? x)

generateBoard
  :: Configuration -> (Position -> a) -> Vector (Vector a)
generateBoard Config{..} act =
  V.generate boardHeight $ \posY ->
    V.generate boardWidth $ \posX ->
      act Pos{..}

generateBoardM
  :: Monad m
  => Configuration -> (Position -> m a) -> m (Vector (Vector a))
generateBoardM Config{..} act =
  V.generateM boardHeight $ \posY ->
    V.generateM boardWidth $ \posX ->
      act Pos{..}

newtype Piece = Piece Bit3
  deriving (Show, Generic, Typeable)
  deriving anyclass (Variable, Boolean, Equatable)

wall :: Maybe Word8 -> Piece
wall (Just 0) = Piece $ encode 0
wall (Just 1) = Piece $ encode 1
wall (Just 2) = Piece $ encode 2
wall (Just 3) = Piece $ encode 3
wall (Just 4) = Piece $ encode 4
wall (Just n) = error $ "Numbered wall must be of range 0-4, but got: " ++ show n
wall Nothing  = Piece $ encode 5

data RawPiece = Wall (Maybe Word8) | Light | Free
  deriving (Read, Show, Eq, Ord)

light, free :: Piece
light = Piece $ encode 6
free  = Piece $ encode 7

type Board = Vector (Vector Piece)
type RawBoard = Vector (Vector RawPiece)

instance Codec Piece where
  type Decoded Piece = RawPiece
  encode (Wall n) = wall n
  encode Light    = light
  encode Free     = free
  decode s (Piece n) = do
    w <- decode s n
    if 0 <= w && w <= 4
    then pure $ Wall $ Just $ fromIntegral w
    else if w == 5
    then pure $ Wall Nothing
    else if w == 6
    then pure Light
    else if w == 7
    then pure Free
    else empty

data Position = Pos { posX :: !Int, posY :: !Int }
  deriving (Eq, Ord, Generic, Typeable)
  deriving anyclass (Hashable)

instance Show Position where
  showsPrec _ Pos{..} = showParen True $
    shows posX . showString ", " . shows posY

instance Num Position where
  fromInteger = Pos <$> fromInteger <*> fromInteger
  Pos x y + Pos x' y' = Pos (x + x') (y + y')
  Pos x y - Pos x' y' = Pos (x - x') (y - y')
  negate (Pos x y) = Pos (negate x) (negate y)
  Pos x y * Pos x' y' = Pos (x * x') (y * y')
  abs (Pos x y) = Pos (abs x) (abs y)
  signum (Pos x y) = Pos (signum x) (signum y)

inBoard :: Configuration -> Position -> Bool
inBoard Config{..} Pos{..} =
  0 <= posX && posX < boardWidth
  && 0 <= posY && posY < boardHeight

perp :: Position -> Position
perp (Pos x y) = Pos y x

getPlacement :: Position -> Configuration -> Maybe WallSpec
getPlacement = lmap walls . HM.lookup

forBoardM_ :: Monad m => Vector (Vector a) -> (Position -> a -> m b) -> m ()
forBoardM_ bd act = V.imapM_ (\posY -> V.imapM_ $ \posX -> act Pos{..}) bd

forBoard :: Vector (Vector a) -> (Position -> a -> b) -> Vector (Vector b)
forBoard bd act = V.imap (\posY -> V.imap $ \posX -> act Pos{..}) bd

initialBoard :: Configuration -> RawBoard
initialBoard cnf = generateBoard cnf $ \pos ->
  maybe Free Wall $ getPlacement pos cnf

render
  :: RawBoard -> String
render = unlines . V.toList . V.map (foldMap renderPiece)

renderPiece :: RawPiece -> String
renderPiece (Wall mn)  =
  "\x1b[7m" ++ maybe ' ' (intToDigit . fromEnum) mn : "\x1b[0m"
renderPiece Free            = " "
renderPiece Light           = "o"

adjacentCells
  :: Configuration -> Position -> [Position]
adjacentCells = stencilCells [Pos (-1) 0, Pos 1 0, Pos 0 (-1), Pos 0 1]

diagonalCells
  :: Configuration -> Position -> [Position]
diagonalCells =
  stencilCells [Pos (-1) (-1), Pos 1 (-1), Pos (-1) 1, Pos 1 1]

stencilCells :: [Position] -> Configuration -> Position -> [Position]
stencilCells windows cfg pos =
  [ p
  | d <- windows
  , let p = pos + d
  , inBoard cfg p && isNothing (getPlacement p cfg)
  ]


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

guardRange
  :: Configuration
  -> Position
  -> Maybe Position
guardRange cfg pos
  | inBoard cfg pos = Just pos
  | otherwise = Nothing

newtype Grid a = Grid { runGrid :: Vector (Vector a) }
  deriving (Functor, Foldable, Traversable, Generic)
instance Wrapped (Grid a)

instance FoldableWithIndex Position Grid where
  ifoldMap f =
    ifoldMap (\posY -> ifoldMap $ \posX -> f Pos{..}) . runGrid
  {-# INLINE ifoldMap #-}

boardSize :: Configuration -> Int
boardSize Config{..} = boardHeight * boardWidth
