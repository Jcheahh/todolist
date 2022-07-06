module Lib (
  someFunc,
) where

data Tags = Home | Online | Power | Offline

data State = Todo' | Started | Next | Waiting | Done

type Clock = Int

type Time = Int

data Todo = Todo
  { title :: String
  , state :: Maybe State
  , tags :: Maybe [Tags]
  , clock :: Maybe Clock
  , time :: Maybe Time
  }

addTodo :: String -> Todo
addTodo x = Todo x Nothing Nothing Nothing Nothing

setState :: State -> Todo -> Todo
setState st (Todo til _ tag cl ti) = Todo til (Just st) tag cl ti

addTags :: Tags -> Todo -> Todo
addTags ta (Todo til st Nothing cl ti) = Todo til st (Just [ta]) cl ti
addTags ta (Todo til st (Just tag) cl ti) = Todo til st (Just (ta : tag)) cl ti

addClock :: Clock -> Todo -> Todo
addClock clock (Todo til state tag _ ti) = Todo til state tag (Just clock) ti

addTime :: Time -> Todo -> Todo
addTime time (Todo til state tag cl _) = Todo til state tag cl (Just time)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
