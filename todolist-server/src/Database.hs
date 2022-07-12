{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Applicative
import Data.Int (Int64)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TodoList = TodoList Int String deriving (Show, Eq)

instance FromRow TodoList where
    fromRow = TodoList <$> field <*> field

instance ToRow TodoList where
    toRow (TodoList id_ str) = toRow (id_, str)

showSpecificTodo :: Connection -> IO ()
showSpecificTodo conn = do
    id <- getLine
    title <- getLine
    jogging <-
        queryNamed
            conn
            "SELECT id,title FROM todo WHERE id = :id AND title = :title"
            [":id" := id, ":title" := title] ::
            IO [TodoList]
    -- print jogging
    mapM_ print jogging

showTodo :: Connection -> IO ()
showTodo conn = do
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r

createTodo :: Connection -> IO ()
createTodo conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS todo (id INTEGER PRIMARY KEY, title TEXT)"
    input <- getLine
    execute
        conn
        "INSERT INTO todo (title) VALUES (?)"
        (Only input)
    -- execute_ conn "INSERT INTO todo (title) VALUES ('Play game')"
    -- execute conn "INSERT INTO todo (id, str) VALUES (?,?)" (TodoList 12 input)
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r

editTodo :: Connection -> IO ()
editTodo conn = do
    id <- getLine
    title <- getLine
    executeNamed
        conn
        "UPDATE todo SET title = :title WHERE id = :id"
        [":title" := title, ":id" := id]
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r

deleteTodo :: Connection -> IO ()
deleteTodo conn = do
    input <- getLine
    execute conn "DELETE FROM todo WHERE id = ?" (Only input)
    r <- query_ conn "SELECT * from todo" :: IO [TodoList]
    mapM_ print r

main :: IO ()
main = do
    conn <- open "todo.db"
    -- tested function
    showSpecificTodo conn
    showTodo conn
    -- createTodo conn
    -- editTodo conn
    -- deleteTodo conn
    close conn
