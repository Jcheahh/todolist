module Lib (
  someFunc,
) where

data Tags = Home | Online | Power | Offline

data State = Todo' | Started | Next | Waiting | Done

type Clock = Int

type Time = Int

data Todo = Todo
  { title :: String
  , state :: State
  , tags :: [Tags]
  , clock :: Clock
  , time :: Time
  }

-- addTodo :: Todo -> [Todo] -> [Todo]
-- addTodo x xs = x : xs

-- addTags :: Todo -> String -> Todo
-- addTags (Todo t i tags) s
--   | s == "offline" = Todo t i (Offline : tags)
--   | s == "online" = Todo t i (Online : tags)
--   | s == "home" = Todo t i (Home : tags)
--   | s == "power" = Todo t i (Power : tags)
--   | otherwise = Todo t i tags

-- toggleIsDone :: Todo -> Todo
-- toggleIsDone (Todo t i tags) = Todo t (not i) tags

someFunc :: IO ()
someFunc = putStrLn "someFunc"
