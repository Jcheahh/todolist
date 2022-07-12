{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Applicative
import Data.Int (Int64)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Test.HUnit

data TestField = TestField Int String deriving (Show, Eq)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance ToRow TestField where
    toRow (TestField id_ str) = toRow (id_, str)

-- data TestEnv = TestEnv
--   { conn :: Connection
--   }

data TodoList = TodoList Int String deriving (Show, Eq)

instance FromRow TodoList where
    fromRow = TodoList <$> field <*> field

instance ToRow TodoList where
    toRow (TodoList id_ str) = toRow (id_, str)

testing1 = TestField 1 "test string"

showTodo :: IO ()
showTodo = do
    conn <- open "todo.db"
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r
    close conn

createTodo :: IO ()
createTodo = do
    conn <- open "todo.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS todo (id INTEGER PRIMARY KEY, title TEXT)"
    input <- getLine
    execute
        conn
        "INSERT INTO todo (title) VALUES (?)"
        (Only input)
    -- execute_ conn "INSERT INTO todo (title) VALUES ('Play game')"
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r
    close conn

editTodo :: IO ()
editTodo = do
    conn <- open "todo.db"
    input <- getLine
    input2 <- getLine
    executeNamed
        conn
        "UPDATE todo SET title = :title WHERE id = :id"
        [":title" := input2, ":id" := input]
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r
    close conn

deleteTodo :: IO ()
deleteTodo = do
    conn <- open "todo.db"
    input <- getLine
    execute conn "DELETE FROM todo WHERE id = ?" (Only input)
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r
    close conn

-- print rowId

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
        [":str" := ("updated str" :: T.Text), ":id" := (1 :: Int)]
    r <- query_ conn "SELECT * from test" :: IO [TestField]
    -- unit test
    -- assertEqual "row count" 6 (length r)
    -- assertEqual "value" testing1 (head r)
    -- jogging <-
    --     queryNamed
    --         conn
    --         "SELECT id,title FROM todo WHERE id = :id AND title = :title"
    --         [":id" := (2 :: Int), ":title" := ("Jogging" :: String)] ::
    --         IO [TodoList]
    -- print jogging
    mapM_ print r
    close conn
