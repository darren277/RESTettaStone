{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UserModel where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), withObject, object)
import Database.PostgreSQL.Simple (Connection, query, execute, Query(..))
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Text (Text)

data User = User
    { id :: Int
    , email :: Text
    } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

instance FromRow User where
    fromRow = User <$> field <*> field

data NewUser = NewUser {
    newEmail :: Text
} deriving Show

instance FromJSON NewUser where
    parseJSON = withObject "NewUser" $ \v ->
        NewUser <$> v .: "email"

instance ToJSON NewUser where
    toJSON (NewUser email) = object ["email" .= email]

insertUser :: Connection -> Text -> IO User
insertUser conn emailVal = do
    [user] <- query conn
        "INSERT INTO users (email) VALUES (?) RETURNING id, email"
        [emailVal]
    return user

getAllUsers :: Connection -> IO [User]
getAllUsers conn =
    query conn "SELECT id, email FROM users ORDER BY id" ()

getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn uid = do
    results <- query conn
        "SELECT id, email FROM users WHERE id = ?"
        [uid]
    case results of
        [user] -> return (Just user)
        _      -> return Nothing

updateUser :: Connection -> Int -> Text -> IO (Maybe User)
updateUser conn uid emailVal = do
    results <- query conn
        "UPDATE users SET email = ? WHERE id = ? RETURNING id, email"
        (emailVal, uid)
    case results of
        [user] -> return (Just user)
        _      -> return Nothing

deleteUser :: Connection -> Int -> IO Bool
deleteUser conn uid = do
    affected <- execute conn
        "DELETE FROM users WHERE id = ?"
        [uid]
    return (affected == 1)
