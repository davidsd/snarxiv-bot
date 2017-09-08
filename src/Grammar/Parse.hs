{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Grammar.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Vector          as V
import           Grammar.Types
import           Prelude              hiding (takeWhile)

ifFailThen :: (Alternative m, Monad m) => m a -> m b -> m b
ifFailThen m1 m2 = do
  r <- (Left <$> m1) <|> (Right <$> m2)
  case r of
    Left  _ -> empty
    Right b -> pure b

takeEscapedWhile :: Char -> (Char -> Bool) -> Parser Text
takeEscapedWhile escapeChar p = scan False watch
  where
    watch False c     | not (p c)       = Nothing
    watch isEscaped c | c == escapeChar = Just (not isEscaped)
    watch isEscaped _                   = Just isEscaped

takeEscapedWhile1 :: Char -> (Char -> Bool) -> Parser Text
takeEscapedWhile1 e p = mfilter (/= "") (takeEscapedWhile e p)

bigSpace :: Parser ()
bigSpace = void (many1 (endOfLine <|> comment))
  where
    comment = void ("#" *> takeTill isEndOfLine)

ident :: Parser Text
ident = takeWhile (inClass "a-zA-Z")

term :: Parser Content
term = fmap Term ("<" *> ident <* ">")

literal :: Parser Content
literal = fmap Literal (txt <|> newLineTxt)
  where
    txt = takeEscapedWhile1 '$' (notInClass "<>|\n")
    newLineTxt = bigSpace *> (decl `ifFailThen` txt)

phrase :: Parser [Content]
phrase = fmap stripPhrase
         (many1 (term <|> literal) <|> emptyPhrase)
  where
    emptyPhrase = mfilter (== '|') peekChar' *> pure []
    stripPhrase = filter (/= Literal "") . stripStart . stripEnd
    stripStart cs = case cs of
      (Literal l : cs') -> Literal (T.stripStart l) : cs'
      cs' -> cs'
    stripEnd cs = case reverse cs of
      (Literal l : cs') -> reverse (Literal (T.stripEnd l) : cs')
      cs' -> reverse cs'

choices :: Parser Choices
choices = fmap V.fromList (phrase `sepBy1` char '|')

decl :: Parser Text
decl = ident <* sep
  where
    sep = hspace >> "::=" >> hspace
    hspace = skipWhile isHorizontalSpace

def :: Parser (Text, Choices)
def = (,) <$> decl <*> choices

grammar :: Parser Grammar
grammar = fmap HM.fromList (def `sepBy1` bigSpace)

grammarFromFile :: FilePath -> IO Grammar
grammarFromFile f = do
  txt <- T.readFile f
  case parseOnly grammar txt of
    Right g -> return g
    Left e  -> error (show e)

