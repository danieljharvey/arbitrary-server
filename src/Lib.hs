{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           Prelude

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Arbitrary.Generic

type UserAPI = "users" :> Get '[JSON] [User]

data Pet = Pet
  { breed  :: String
  , petAge :: Integer
  , isGood :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Pet

instance Arbitrary Pet where
  arbitrary = genericArbitrary

data User = User
  { name               :: String
  , age                :: Int
  , email              :: String
  , percentageComplete :: Double
  , pets               :: [Pet]
  } deriving (Eq, Show, Generic)

instance ToJSON User

instance Arbitrary User where
  arbitrary = genericArbitrary



server :: Server UserAPI
server = do
  liftIO $ sample' arbitrary


userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app
