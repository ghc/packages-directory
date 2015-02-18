{-# LANGUAGE CPP #-}
-- Simplistic test suite for now. Worthwhile to add a dependency on a
-- test framework at some point.
module Main (main) where
import Data.List (sort)
import System.Directory
import System.FilePath (normalise)
import System.IO.Error (catchIOError)
import TestUtils

#define CHECK_EQ(expected, actual) \
  (checkEq __FILE__ __LINE__ (expected) (actual))

main :: IO ()
main = do
  let expected = sort [".", "..", "main.hs", "TestUtils.hs"]
  actual <- sort `fmap` getDirectoryContents "test"
  CHECK_EQ(expected, actual)

  testRemoveDirectoryRecursive

checkEq :: (Eq a, Show a) => String -> Int -> a -> a -> IO ()
checkEq filename lineNum expected actual
  | expected == actual = return ()
  | otherwise          =
      ioError . userError $
      "failure (" ++ filename ++ ":" ++ show lineNum ++ "):\n  expected " ++
      showsPrec 11 expected "" ++ "\n  but got  " ++
      showsPrec 11 actual " instead"

testRemoveDirectoryRecursive :: IO ()
testRemoveDirectoryRecursive = do

  ------------------------------------------------------------
  -- clean up junk from previous invocations

  modifyPermissions (normalise "TMP/c") (\ p -> p { writable = True })
    `catchIOError` \ _ -> return ()
  removeDirectoryRecursive (normalise "TMP")
    `catchIOError` \ _ -> return ()

  ------------------------------------------------------------
  -- set up

  createDirectoryIfMissing True (normalise "TMP/a/x/w")
  createDirectoryIfMissing True (normalise "TMP/a/y")
  createDirectoryIfMissing True (normalise "TMP/a/z")
  createDirectoryIfMissing True (normalise "TMP/b")
  createDirectoryIfMissing True (normalise "TMP/c")
  writeFile (normalise "TMP/a/x/w/u") (normalise "foo")
  writeFile (normalise "TMP/a/t")     (normalise "bar")
  tryCreateSymbolicLink (normalise "../a") (normalise "TMP/b/g")
  tryCreateSymbolicLink (normalise "../b") (normalise "TMP/c/h")
  tryCreateSymbolicLink (normalise "a")    (normalise "TMP/d")
  modifyPermissions (normalise "TMP/c") (\ p -> p { writable = False })

  ------------------------------------------------------------
  -- tests

  getDirectoryContents (normalise "TMP")   >>= \ contents ->
    CHECK_EQ(sort (words ". .. a b c d"), sort contents)
  getDirectoryContents (normalise "TMP/a") >>= \ contents ->
    CHECK_EQ(sort (words ". .. t x y z"), sort contents)
  getDirectoryContents (normalise "TMP/b") >>= \ contents ->
    CHECK_EQ(sort (words ". .. g"), sort contents)
  getDirectoryContents (normalise "TMP/c") >>= \ contents ->
    CHECK_EQ(sort (words ". .. h"), sort contents)
  getDirectoryContents (normalise "TMP/d") >>= \ contents ->
    CHECK_EQ(sort (words ". .. t x y z"), sort contents)

  removeDirectoryRecursive (normalise "TMP/d")
    `catchIOError` \ _ -> removeFile      (normalise "TMP/d")
#ifdef mingw32_HOST_OS
    `catchIOError` \ _ -> removeDirectory (normalise "TMP/d")
#endif

  getDirectoryContents (normalise "TMP")   >>= \ contents ->
    CHECK_EQ(sort (words ". .. a b c"), sort contents)
  getDirectoryContents (normalise "TMP/a") >>= \ contents ->
    CHECK_EQ(sort (words ". .. t x y z"), sort contents)
  getDirectoryContents (normalise "TMP/b") >>= \ contents ->
    CHECK_EQ(sort (words ". .. g"), sort contents)
  getDirectoryContents (normalise "TMP/c") >>= \ contents ->
    CHECK_EQ(sort (words ". .. h"), sort contents)

  removeDirectoryRecursive (normalise "TMP/c")
    `catchIOError` \ _ -> do
      modifyPermissions (normalise "TMP/c") (\ p -> p { writable = True })
      removeDirectoryRecursive (normalise "TMP/c")

  getDirectoryContents (normalise "TMP")   >>= \ contents ->
    CHECK_EQ(sort (words ". .. a b"), sort contents)
  getDirectoryContents (normalise "TMP/a") >>= \ contents ->
    CHECK_EQ(sort (words ". .. t x y z"), sort contents)
  getDirectoryContents (normalise "TMP/b") >>= \ contents ->
    CHECK_EQ(sort (words ". .. g"), sort contents)

  removeDirectoryRecursive (normalise "TMP/b")

  getDirectoryContents (normalise "TMP")   >>= \ contents ->
    CHECK_EQ(sort (words ". .. a"), sort contents)
  getDirectoryContents (normalise "TMP/a") >>= \ contents ->
    CHECK_EQ(sort (words ". .. t x y z"), sort contents)

  removeDirectoryRecursive (normalise "TMP/a")

  getDirectoryContents (normalise "TMP")   >>= \ contents ->
    CHECK_EQ(sort (words ". .."), sort contents)

  ------------------------------------------------------------
  -- clean up

  removeDirectoryRecursive (normalise "TMP")
