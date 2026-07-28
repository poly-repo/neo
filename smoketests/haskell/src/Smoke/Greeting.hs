module Smoke.Greeting
  ( activeNames,
    renderGreeting,
  )
where

import Data.List (sort)

import Smoke.User (User (..))

renderGreeting :: User -> String
renderGreeting user = "Hello, " ++ userName user

activeNames :: [User] -> [String]
activeNames users =
  -- Intentional HLint findings: redundant lambda and comparison with True.
  map (\user -> userName user) (filter (\user -> userActive user == True) users)
