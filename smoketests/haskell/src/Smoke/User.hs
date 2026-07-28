{-# LANGUAGE DerivingStrategies #-}

module Smoke.User
  ( User (..)
  , defaultUser
  ) where

data User = User
  { userName :: String
  , userAge :: Int
  , userActive :: Bool
  }
  deriving stock (Eq, Show)

defaultUser :: User
defaultUser =
  User
    { userName = "Ada"
    -- Intentional GHC/HLS type error: userAge requires Int.
    , userAge = "forty-two"
    , userActive = True
    }
