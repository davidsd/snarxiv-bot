{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grammar.Run where

import Data.Char
import Control.Monad
import Grammar.Types
import Data.Text (Text)
import Data.Text.Titlecase
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.Random


randomElt :: Vector a -> IO a
randomElt v = do
  i <- randomRIO (0, V.length v - 1)
  return (v V.! i)

run :: Grammar -> Text -> IO Text
run grammar term = do
  phrase <- randomElt (grammar HM.! term)
  fmap T.concat $ forM phrase $ \case
    Term t -> run grammar t
    Literal l -> return l

cleanText :: Text -> Text
cleanText =
  foldr (.) id (reverse
                [ T.unwords . T.words
                , T.replace " ," ","
                , T.replace " ." "."
                , T.replace " ;" ";"
                , T.replace " :" ":"
                , capitalizeSentences
                ]) 

sentences :: Text -> [Text]
sentences = filter (/= "") . map T.strip . T.splitOn ". " . flip T.snoc ' '

unSentences :: [Text] -> Text
unSentences = T.unwords . map (flip T.snoc '.')

capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Just (a, bs) -> T.cons (toUpper a) bs
  Nothing      -> ""

capitalizeSentences :: Text -> Text
capitalizeSentences = unSentences . map capitalize . sentences

runClean :: Grammar -> Text -> IO Text     
runClean g = fmap cleanText . run g

runTitle :: Grammar -> Text -> IO Text
runTitle g = fmap (T.dropEnd 1 . T.pack . titlecase . T.unpack) . runClean g

