{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Main (main) where
import Foreign (Ptr)
import Foreign.C (CChar(..), CInt(..), withCString)
import Data.Functor ((<$>))
import System.Directory ()    -- make sure `directory` is built beforehand
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)

main :: IO ()
main =
  exitWith =<<
  rawSystem "cabal" .         -- execute in the Cabal sandbox environment
  (["exec", "--", "sh", "testsuite/run"] ++) =<<
  getArgs

-- we can't use the `process` library as it causes a dependency cycle with
-- Cabal, so we reinvent the wheel here in a simplistic way; this will
-- probably break with non-ASCII characters on Windows
rawSystem :: String -> [String] -> IO ExitCode
rawSystem cmd args  =
  withCString (unwords (quoteArgument <$> cmd : args)) $ \ c_command ->
  makeExitCode . fromIntegral <$> c_system c_command

makeExitCode :: Int -> ExitCode
makeExitCode 0 = ExitSuccess
makeExitCode e = ExitFailure e

-- handle the different quoting rules in CMD.EXE vs POSIX shells
quoteArgument :: String -> String
#ifdef mingw32_HOST_OS
quoteArgument s = "\"" ++ replaceElem '"' "\"\"" s ++ "\""
#else
quoteArgument s = "'" ++ replaceElem '\'' "'\\''" s ++ "'"
#endif

replaceElem :: Eq a => a -> [a] -> [a] -> [a]
replaceElem match repl = concat . (replace <$>)
  where replace c | c == match = repl
                  | otherwise  = [c]

foreign import ccall safe "stdlib.h system" c_system :: Ptr CChar -> IO CInt
