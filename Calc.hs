import System.Environment
import AbstractInteger

main = do
   args <- getArgs
   print $ solveRPN args
