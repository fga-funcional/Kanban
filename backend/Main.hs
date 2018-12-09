{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Aeson (ToJSON, FromJSON, decode, eitherDecode, encode)
import GHC.Generics
import Web.Scotty
import Network.Wai.Middleware.Cors
import qualified Data.ByteString.Lazy as B

data Status = Backlog | Todo | Doing | Done | Archived deriving (Show, Eq, Generic)
instance FromJSON Status
instance ToJSON Status

newtype IssueList = IssueList [Issue] deriving (Generic)
instance FromJSON IssueList
instance ToJSON IssueList

data Issue = Issue {
    description :: String,
    status :: String
} deriving (Show, Eq, Generic)
instance FromJSON Issue
instance ToJSON Issue

fromFile :: IO B.ByteString
fromFile = B.readFile "kanban.json"

getIssueList :: Maybe IssueList
getIssueList =
  -- let
  --   x = eitherDecode <$> fromFile :: IO (Either String IssueList)
  -- in
  --   case x of
  --     IO Left  _ -> Nothing
  --     IO Right y -> Just y
  decode example :: Maybe IssueList

main :: IO ()
main = do
  scotty 3000 $ do
    middleware simpleCors
    get "/issues" $ do
      json getIssueList

example =
  "[{ \"description\": \"Dividir em 5 boards\", \"status\": \"done\" }, { \"status\": \"doing\", \"description\": \"Ler JSON de arquivo\" }, { \"status\": \"todo\", \"description\": \"Exportar para JSON\" }, { \"status\": \"backlog\", \"description\": \"esconder os boards Backlog e Archived\" }, { \"status\": \"todo\", \"description\": \"Dormir mais de 3h por noite\" }, { \"status\": \"doing\", \"description\": \"Finalizar semestre (ou ser finalizado)\" }, { \"status\": \"doing\", \"description\": \"Tentar aprender Haskell de novo\" }, { \"status\": \"done\", \"description\": \"Tentar aprender Haskell\" }, { \"status\": \"archived\", \"description\": \"Colocar umas issues em arquivo JSON\" }, { \"status\": \"archived\", \"description\": \"Mudar de curso\" }, { \"status\": \"archived\", \"description\": \"Fazer dieta\" }, { \"status\": \"backlog\", \"description\": \"Comprar ventilador\" } ]"