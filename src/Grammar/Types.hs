module Grammar.Types where

import           Data.HashMap.Strict
import           Data.Text
import           Data.Vector

data Content = Literal Text | Term Text
             deriving (Show, Eq)

type Choices = Vector [Content]

type Grammar = HashMap Text Choices
