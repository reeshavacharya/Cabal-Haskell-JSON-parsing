{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), Object)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Data.ByteString.Lazy as BL

newtype Content
  = Content {name :: Text}
  deriving Show

newtype Payload
  = Payload {content :: [Content]}
  deriving Show

newtype NFT
  = NFT {payload :: Payload}
  deriving Show

instance JSON.FromJSON Content where
       parseJSON (JSON.Object v) = 
              Content <$> v .: "name"

instance JSON.FromJSON Payload where
       parseJSON (JSON.Object o) = 
              Payload <$> o .: "content"  

instance JSON.FromJSON NFT where
       parseJSON (JSON.Object v) = 
              NFT <$> v .: "payload"

getNFTContent :: IO BL.LazyByteString
getNFTContent = do 
                manager <- newTlsManager
                request <- HTTP.parseRequest "https://artano.io/api/nft/nfts/discoverable"
                HTTP.responseBody <$> HTTP.httpLbs request manager

main :: IO ()
main = do 
       nft <- getNFTContent
       case JSON.decode nft :: Maybe NFT of
              Just a -> mapM_ ( print.name) $ content (payload a)
              _ -> putStrLn "Nothing"
