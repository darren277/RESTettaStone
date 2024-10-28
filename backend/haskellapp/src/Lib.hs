module Lib (runApp) where

import Control.Monad
import Control.Exception
import Data.Char
import System.IO
import Network.Socket
import System.Environment
import Data.Time.LocalTime
import Text.Read

data RequestType = GET | POST deriving (Show)
data Request = Request { rtype :: RequestType, path :: String, options :: [(String,String)] }
data Response = Response { version :: String, statuscode :: Int }

instance Show Request where
        show r = "Request { " ++ show((rtype r)) ++ " " ++ (path r)  ++ (foldl (\acc (k,v) -> acc ++ "\n  " ++ k ++ ": " ++ v) "" (options r)) ++ "\n}"

instance Show Response where
        show r = version(r) ++ " " ++ show(statuscode(r)) ++ " " ++ (case statuscode(r) of
                100 -> "Continue"
                200 -> "OK"
                404 -> "Not Found") ++ "\r\n\r\n"

fromString :: String -> RequestType
fromString t = case t of
        "GET" -> GET
        "POST" -> POST

respond :: Request -> Handle -> IO ()
respond request handle = do
        putStrLn $ show request
        let response = Response {version = "HTTP/1.1", statuscode = 200}
        hPutStr handle $ show(response)
        time <- getZonedTime
        hPutStr handle $ "Haskell says HELLO.\nThe time is currently " ++ show(time) ++ "\n\n\nHere is some info from your session:\n" ++ show(request)

--- This should really validate input or something. Separate validator? Or as-we-go?
parseRequestHelper :: ([String], [(String,String)]) -> [(String,String)]
parseRequestHelper ([], accum) = accum
parseRequestHelper ((l:rest), accum)
        | (length (words l)) < 2 = accum
        | otherwise = parseRequestHelper(rest, accum ++ [(reverse . tail . reverse . head . words $ l, unwords . tail . words $ l)] )

parseRequest :: [String] -> Request
parseRequest lns = case (words (head lns)) of
        [t,p,_] -> Request {rtype=(fromString t), path=p, options=parseRequestHelper((tail lns),[])}

handleAccept :: Handle -> String -> IO ()
handleAccept handle hostname = do
    putStrLn $ "Handling request from " ++ hostname
    requestLines <- gatherRequest handle []
    let request = parseRequest requestLines
    respond request handle
    putStrLn $ "BEFORE hClose handle"
    hClose handle  -- Ensure handle is closed after the response is sent
    putStrLn "Connection closed"

-- Helper function to gather all lines until an empty line is found
gatherRequest :: Handle -> [String] -> IO [String]
gatherRequest handle lines = do
    line <- hGetLine handle
    if null line
        then return (reverse lines)
        else gatherRequest handle (line : lines)

runApp :: IO ()
runApp = withSocketsDo $ do
    -- Attempt to read the port number from the environment variable "PORT"
    maybePort <- lookupEnv "PORT"
    let port = case maybePort >>= readMaybe of
            Just p -> p  -- Use the environment variable if available and valid
            Nothing -> 3014  -- Default to 3014 if not found or invalid

    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock 5
    putStrLn $ "Listening on port " ++ show port
    forever $ do
        (connSock, SockAddrInet _ _) <- accept sock
        connHandle <- socketToHandle connSock ReadMode
        handleAccept connHandle "unknown"
        hClose connHandle
