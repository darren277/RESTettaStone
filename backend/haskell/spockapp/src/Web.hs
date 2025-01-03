{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web where

import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)

import qualified Database.PostgreSQL.Simple as PG
import Data.Pool (Pool)

import System.Envy (decodeEnv, envMaybe, FromEnv(fromEnv), DefConfig(defConfig), (.!=))

import Database (initDbPool, DbConfig)
import AppTypes (AppState(..))
import WebUserRoutes (Api, userRoutes)

type ApiApp = SpockM () () AppState ()

data Config = Config
    { port     :: Int
    , dbConfig :: DbConfig
    } deriving (Show, Generic)

instance DefConfig Config where
    defConfig = Config
        { port     = 8080
        , dbConfig = defConfig
        }

instance FromEnv Config where
    fromEnv _ = Config
        <$> envMaybe "PORT" .!= 8080
        <*> fromEnv Nothing

app :: IO ()
app = do
    configResult <- decodeEnv :: IO (Either String Config)
    case configResult of
        Left err -> error $ "Failed to load config: " ++ err
        Right Config { port, dbConfig } -> do
            pool <- initDbPool dbConfig
            spockCfg <- defaultSpockCfg () PCNoDatabase (AppState pool)
            runSpock port (spock spockCfg appRoutes)

appRoutes :: Api
appRoutes = do
    get root $ text "Hello, world!"
    AppState pool <- getState
    userRoutes pool
