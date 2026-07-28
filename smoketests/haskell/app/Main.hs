module Main (main) where

import Smoke.Greeting (activeNames, renderGreeting)
import Smoke.User (defaultUser)

main :: IO ()
main = do
  putStrLn (renderGreeting defaultUser)
  print (activeNames [defaultUser])
