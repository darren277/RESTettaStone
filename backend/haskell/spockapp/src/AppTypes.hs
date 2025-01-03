module AppTypes where

import Data.IORef
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG

data AppState = AppState
    { dbPool :: Pool PG.Connection }
