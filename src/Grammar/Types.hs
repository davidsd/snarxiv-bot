module Grammar.Types where

import Data.Text
import Data.Vector
import Data.HashMap.Strict

data Content = Literal Text | Term Text
             deriving (Show, Eq)

type Choices = Vector [Content]

type Grammar = HashMap Text Choices
