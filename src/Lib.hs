{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib where

import Prelude hiding (takeWhile)
import Control.Applicative

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C

data Quantifier
    = Distinct
    | All
    | NoQuantifier
    deriving (Show)

data Everything = Everything
    deriving (Show)

data Value t where
    S      :: C.ByteString -> Value C.ByteString
    Concat :: Value C.ByteString -> Value C.ByteString -> Value C.ByteString
    Null   :: Value (Maybe a)
deriving instance Show (Value t)

data DerivedColumn where
    DerivedValue :: Value t -> DerivedColumn
deriving instance Show DerivedColumn

data TableRef = TableName C.ByteString
    deriving (Show)

data SelectQuery = SelectQuery
    { _selectQueryQuantifier :: Quantifier
    , _selectQueryList :: Either Everything [DerivedColumn]
    , _selectQueryFrom :: [TableRef]
    }
    deriving (Show)

-- General combinators
chainr1 p op = (flip ($) <$> p <*> op <*> chainr1 p op)
    <|> p

word s = stringCI s <* takeWhile isSpace

-- Grammar
quantifier = (word "distinct" *> pure Distinct)
    <|> (word "all" *> pure All)
    <|> pure NoQuantifier

strVal = chainr1 s op
    where s = S <$> (char '"' *> takeWhile (not . inClass "\"\\") <* char '"' <* skipSpace)
          op = word "||" *> pure Concat

nullVal = word "null" *> pure Null

val = (DerivedValue <$> strVal)
    <|> (DerivedValue <$> nullVal)

selectList = (word "*" *> pure (Left Everything))
    <|> (Right <$> sepBy1 val (char ',' <* takeWhile isSpace))

table = (:[]) . TableName <$> (word "from" *> takeWhile1 (inClass "a-zA-Z"))

select = SelectQuery
    <$> (word "select" *> quantifier)
    <*> selectList
    <*> table

someFunc :: IO ()
someFunc = putStrLn "someFunc"
