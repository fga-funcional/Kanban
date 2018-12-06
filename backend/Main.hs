
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics
import Web.Scotty
import Network.Wai.Middleware.Cors
import qualified Data.ByteString.Lazy as B

data Status = Backlog | Todo | Doing | Done | Archived deriving (Show, Eq, Generic)
instance FromJSON Status
instance ToJSON Status

data Issue = Issue {
    description :: String,
    status :: Status
} deriving (Show, Eq, Generic)
instance FromJSON Issue
instance ToJSON Issue

jsonFile :: FilePath
jsonFile = "kanban.json"

type Description = String
type Status = String

allIssues :: M.Map Status [Description]
allIssues = M.fromList
  [ ( "done", 
      [ "Dividr em 5 boards"
      , "Criar arquivdar e desarquivar"
      ]
    )
  , ( "doing",
      [ "Importar de JSON"
      , "Integrar com o Haskell"  
      ]
    )
  , ( "todo",
      [ "Exportar para JSON"
      , "Deixar mais bonito"
      ]
    )
  , ( "backlog",
      [ "Criar label para as issues"
      , "Criar comentáro na issue"
      ]
    ) 
  ]

main :: IO ()
main = do
    issues' <- newMVar  allIssues

    scotty 3000 $ do
        middleware simpleCors

        get "/issues" $ do
          issues <- liftIO $ readMVar issues'
          json issues

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
    scotty 3000 $ do
        middleware simpleCors
        get "/" $ do
            html "Hello, Kanban!"

