{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty as S

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
