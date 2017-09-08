{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified Grammar.Parse      as Parse
import           Grammar.Run
import           System.Environment
import           System.Random
import           Twitter.Bot

getSnarxivId :: IO Text
getSnarxivId = do
  time <- getCurrentTime
  let dateId = formatTime defaultTimeLocale "%y%m" time
  paperId <- fmap (concatMap show) (replicateM 5 (randomRIO (0 :: Int, 9)))
  return (T.pack (dateId ++ "." ++ paperId))

getTweet :: FilePath -> IO Text
getTweet grammarFile = do
  g <- Parse.grammarFromFile grammarFile
  authors <- fmap (T.dropEnd 1) (runClean g "authors")
  title   <- runTitle g "title"
  snarxivId <- getSnarxivId
  return ("[" <> snarxivId <> "] " <> authors <> ": " <> title)

main :: IO ()
main = do
  [grammarFile] <- getArgs
  getTweet grammarFile >>= tweet
