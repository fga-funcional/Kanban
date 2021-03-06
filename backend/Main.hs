{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Aeson (ToJSON, FromJSON, decode, eitherDecode, encode)
import GHC.Generics
import Web.Scotty as S
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO.Strict as Strict

data Status = Backlog | Todo | Doing | Done | Archived deriving (Show, Eq, Generic)
instance FromJSON Status
instance ToJSON Status

newtype IssueList = IssueList [Issue] deriving (Generic)
instance FromJSON IssueList
instance ToJSON IssueList

data Issue = Issue {
    description :: String,
    status :: Status
} deriving (Show, Eq, Generic)
instance FromJSON Issue
instance ToJSON Issue

getFile :: IO String
getFile = do
  let filePath = "kanban.json"
  contents <- Strict.readFile filePath
  initFile filePath contents
  return filePath

initFile :: FilePath -> String -> IO ()
initFile path _ = mempty

saveToJsonFile :: String -> IO String
saveToJsonFile contents = do
  let filePath = "kanban.json"
  writeFile filePath contents
  return filePath

main :: IO ()
main = do
  scotty 3000 $ do
    middleware simpleCors
    get "/issues" $ do
      filePath <- liftIO $ getFile
      file filePath

    post "/api" $ do
      body <- S.body 
      filePath <- liftIO $ saveToJsonFile (B.unpack body) 
      file filePath 
