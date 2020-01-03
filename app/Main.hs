{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import           Ersatz
import           Puzzles.Akari
import           System.Environment
import           Text.Printf

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
  args <- getArgs
  input <- case args of
    []    -> return example
    ["-"] -> parseConfig <$> BS.getContents
    [fp]  -> parseConfig <$> BS.readFile fp
    _ -> error "Argument must be empty (solves example), \"-\" (read from stdin), or single filepath."
  putStrLn "Solving: "
  putStrLn $ render $ initialBoard input
  putStrLn "After applying heuristics:"
  let partial = applyHeuristics defaultHeuristics input
      empties = boardSize input - HM.size (walls input)
      solveds = HM.size partial - HM.size (walls input)
      ratio = fromIntegral solveds / fromIntegral empties * 100 :: Double
  putStrLn $ renderPartial input  partial
  putStrLn $
    unlines
      [ unwords
        [show solveds, "/", show empties, "cell(s) are filled by heuristics"
        ,"("++ printf "%.02f" ratio ++ "%)."
        ]
      , ""
      , "----"
      ]

  (res, msol) <- solveWith cryptominisat5 (problemWith partial input)
  if HM.size partial == boardWidth input * boardHeight input
    then if res == Unsatisfied
         then error $ "Solution given by heuristics is wrong!\n\t" ++ show msol
         else putStrLn "We successfuly solved the puzzle only with heuristics!"
    else do
      when (res /= Satisfied) (fail (show res))
      case msol of
        []  -> fail ("No solution found: " ++ show msol)
        sols -> do
          putStrLn $ show (length sols) ++ " solution(s) found by SAT: "
          iforM_ sols $ \i sol -> do
            let lab = '#' : show i
            putStrLn $ lab ++ ' ' : replicate (boardWidth input - length lab) '-'
            putStrLn $ render sol
