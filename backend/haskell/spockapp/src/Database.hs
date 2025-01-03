{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as PG
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Pool
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Envy as Envy
import GHC.Generics

data DbConfig = DbConfig
    { dbHost :: String
    , dbPort :: Int
    , dbUser :: String
    , dbPass :: String
    , dbName :: String
    } deriving (Generic, Show)

instance Envy.DefConfig DbConfig where
    defConfig = DbConfig "localhost" 5432 "postgres" "postgres" "postgres"

instance Envy.FromEnv DbConfig where
    fromEnv _ = DbConfig
        <$> Envy.envMaybe "PG_HOST" Envy..!= "localhost"
        <*> Envy.envMaybe "PG_PORT" Envy..!= 5432
        <*> Envy.envMaybe "PG_USER" Envy..!= "postgres"
        <*> Envy.envMaybe "PG_PASS" Envy..!= "postgres"
        <*> Envy.envMaybe "PG_DB"   Envy..!= "postgres"

-- Create a connection string from config
createConnString :: DbConfig -> PG.ConnectInfo
createConnString DbConfig{..} = PG.ConnectInfo
    { PG.connectHost = dbHost
    , PG.connectPort = fromIntegral dbPort
    , PG.connectUser = dbUser
    , PG.connectPassword = dbPass
    , PG.connectDatabase = dbName
    }

initDbPool :: DbConfig -> IO (Pool PG.Connection)
initDbPool config = createPool
    (PG.connect $ createConnString config)
    PG.close
    1  -- number of stripes
    10 -- timeout in seconds
    10 -- max connections per stripe

withConn :: Pool PG.Connection -> (PG.Connection -> IO a) -> IO a
withConn = withResource
