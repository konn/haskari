{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Control.Lens
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import           Ersatz
import           Puzzles.Akari

-- | Example problem from
-- <http://www.nikoli.co.jp/en/puzzles/akari.html Nikoli's site>
example :: Configuration
example = Config { boardHeight =7, boardWidth = 7, walls}
  where
    walls =
      HM.fromList
        [ (Pos 2 0, Nothing), (Pos 6 0, Nothing)
        , (Pos 1 1, Just 4), (Pos 4 1,Just 1), (Pos 6 1, Nothing)
        , (Pos 3 2, Just 2)
        , (Pos 1 3, Nothing), (Pos 5 3, Nothing)
        , (Pos 3 4, Nothing)
        , (Pos 0 5, Nothing), (Pos 2 5, Nothing), (Pos 5 5, Just 1)
        , (Pos 0 6, Just 1), (Pos 4 6, Just 1)
        ]

main :: IO ()
main = do
  putStrLn "Solving: "
  putStrLn $ render $ initialBoard example
  (res, msol) <- solveWith cryptominisat5 (problem example)
  when (res /= Satisfied) (fail (show res))
  case msol of
    []  -> fail ("No solution found: " ++ show msol)
    sols -> do
      putStrLn $ show (length sols) ++ " solution(s) found: "
      iforM_ sols $ \i sol -> do
        let lab = '#' : show i
        putStrLn $ lab ++ ' ' : replicate (boardWidth example - length lab) '-'
        putStrLn $ render sol
