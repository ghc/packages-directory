module Main (main) where
import System.Directory ()
import System.Environment (getArgs)
import System.Process (callProcess)

main :: IO ()
main =
  -- execute in the Cabal sandbox environment
  callProcess "cabal" . (["exec", "--", "sh", "testsuite/run"] ++) =<< getArgs
