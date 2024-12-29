{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebUserRoutes where

import Web.Spock
import Web.Spock.Config
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as PG
import Web.Spock (SpockM, ActionCtxT)
import Data.Aeson ((.=), object)
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)

import Database (initDbPool, withConn)
import UserModel

import AppTypes (AppState(..))

type Api = SpockM () () AppState ()
type ApiAction a = ActionCtxT () (WebStateM () () AppState) a

userRoutes :: Pool PG.Connection -> Api
userRoutes pool = do

    -- GET /users
    get "users" $ do
        users <- liftIO $ withConn pool getAllUsers
        json users

    -- POST /users
    post "users" $ do
        newUserData <- jsonBody' :: ApiAction NewUser
        inserted <- liftIO $ withConn pool $ \conn ->
            insertUser conn (newEmail newUserData)
        json inserted

    -- GET /users/:id
    get ("users" <//> var) $ \userId -> do
        maybeUser <- liftIO $ withConn pool $ \conn ->
            getUserById conn userId
        case maybeUser of
            Just user -> json user
            Nothing   -> setStatus status404 >> json (object ["error" .= ("User not found" :: Text)])

    -- PUT /users/:id
    put ("users" <//> var) $ \userId -> do
        newUserData <- jsonBody' :: ApiAction NewUser
        maybeUpdated <- liftIO $ withConn pool $ \conn ->
            updateUser conn userId (newEmail newUserData)
        case maybeUpdated of
            Just user -> json user
            Nothing   -> setStatus status404 >> json (object ["error" .= ("User not found" :: Text)])

    -- DELETE /users/:id
    delete ("users" <//> var) $ \userId -> do
        deleted <- liftIO $ withConn pool $ \conn ->
            deleteUser conn userId
        if deleted
            then json $ object ["id" .= userId]
            else setStatus status404 >> json (object ["error" .= ("User not found" :: Text)])
