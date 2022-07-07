{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test where

import Control.Applicative
import Data.Int (Int64)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Test.HUnit

data TestField = TestField Int String deriving (Show, Eq)

-- data TestEnv = TestEnv
--   { conn :: Connection
--   }

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

xxx :: Int64
xxx = 1

testing1 = TestField 1 "test string"

testSimpleSelect :: IO ()
testSimpleSelect = do
  conn <- open "test.db"
  -- execute_ conn "CREATE TABLE test1 (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string 3')"
  rowId <- lastInsertRowId conn
  execute conn "DELETE FROM test1 WHERE id = ?" (Only rowId)
  -- rows <- query_ conn "SELECT t FROM test1" :: IO [Only String]
  -- assertEqual "row count" 1 (length rows)
  -- assertEqual "string" (Only "test string") (head rows)
  r <- query_ conn "SELECT * from test1" :: IO [TestField]
  mapM_ print r
  print rowId

main :: IO ()
main = do
  conn <- open "test.db"
  -- execute_ conn "INSERT INTO test (str) VALUES ('test string 2')"
  execute
    conn
    "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: String))
  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  -- execute_ conn "DELETE FROM test WHERE id = 2"
  -- execute conn "DELETE FROM test WHERE id = ?" (Only xxx)
  executeNamed
    conn
    "UPDATE test SET str = :str WHERE id = :id"
    [":str" := ("updated str" :: T.Text), ":id" := xxx]
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  -- unit test
  -- assertEqual "row count" 6 (length r)
  -- assertEqual "value" testing1 (head r)
  mapM_ print r
  close conn
