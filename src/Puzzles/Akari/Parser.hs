{-# LANGUAGE RecordWildCards, TupleSections #-}
module Puzzles.Akari.Parser where
import qualified Control.Foldl               as L
import           Control.Lens
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Strict.Lens
import           Data.Char
import qualified Data.HashMap.Strict         as HM
import           Data.MonoTraversable
import           Puzzles.Akari.Types

parseConfig :: BS.ByteString -> Configuration
parseConfig src =
  let bs = BS.lines src
      boardHeight = length bs
      boardWidth = maximum $ 0 : map BS.length bs
      walls = parseWalls src
  in Config{..}

parseWalls :: BS.ByteString -> Walls
parseWalls = parseBoardWith
  $ HM.fromList
  $ [('*', Nothing), ('■', Nothing)]
      ++
    [ (intToDigit i, Just $ fromIntegral i)
    | i <- [0..4]
    ]

ofolded
  :: (Contravariant f, Applicative f, MonoFoldable mono)
  => (Element mono -> f (Element mono)) -> (mono -> f mono)
ofolded k ts = contramap (const ()) (otraverse_ k ts)

parseBoardWith
  :: HM.HashMap Char a -> BS.ByteString -> HM.HashMap Position a
parseBoardWith dic =
  L.foldOver (ifolded.withIndex)
    ( lmap
      (\(posY, bs) ->
        L.foldOver (chars.withIndex)
        (lmap (\(posX, c) -> (Pos{..},) <$> HM.lookup c dic)
        $ L.handles _Just L.hashMap
        )
        bs
      )
    L.mconcat
    )
  . BS.lines
