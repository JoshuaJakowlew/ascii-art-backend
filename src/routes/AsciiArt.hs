{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Routes.AsciiArt ( getAsciiArt
                       , postAsciiArt
                       ) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.Functor                 (($>))
import           Data.ByteString        as BS (ByteString, writeFile)
import           Data.Text                    (Text, pack, unpack)
import           GHC.Generics

import           Data.Aeson            hiding (json)                        
import           Data.GUID                    (genString)
import           Data.HashMap.Strict    as HM (HashMap, lookup)
import           Network.Curl.Download        (openURI)
import           System.Process               (system)
import           Web.Spock
import Data.Maybe(fromJust)

import           Config

data AsciiArtRequest = InvalidRequest
                     | AsciiArtRequest { urlParam :: Text
                                       } deriving(Show)

data AsciiArtResponse = AsciiArtResponse { success :: Bool
                                         , art     :: Maybe Text
                                         } deriving(Generic, Show)

instance ToJSON   AsciiArtResponse


getAsciiArt :: ApiAction ()
getAsciiArt = getUrl >>= loadFile >>= sendArt
    where
        getUrl = unpack . urlParam <$> getParams

postAsciiArt :: ApiAction ()
postAsciiArt = getUrl >>= loadFile >>= sendArt
    where
        getUrl = ( ("file:///" ++)
                 . uf_tempLocation
                 . fromJust
                 . HM.lookup "image") <$> files



-- HANDLE REQUESTS PARAMS


getParams :: ApiAction AsciiArtRequest
getParams = buildRequest <$> param "url"
    where
        buildRequest (Just url) = AsciiArtRequest url
        buildRequest Nothing    = InvalidRequest

sendArt :: Maybe ByteString -> ApiAction ()
sendArt (Just bytes) = do
    aArt <- pack <$> (liftIO $ processImage bytes)
    sendJSON True (Just aArt)
sendArt Nothing      =
    sendJSON False Nothing

processImage :: ByteString -> IO (String)
processImage bytes = writeTempFile bytes >>= runConverter >>= readFile            

sendJSON :: Bool -> Maybe Text -> ApiAction ()
sendJSON success' art' = json $ AsciiArtResponse { success = success', art = art' }



-- PROCESS IMAGE


loadFile :: String -> ApiAction (Maybe ByteString)
loadFile url = getBytes <$> (liftIO . openURI $ url)
    where
        getBytes (Right bytes) = Just bytes
        getBytes _             = Nothing

writeTempFile :: ByteString -> IO String
writeTempFile bytes = genString >>= \uuid -> BS.writeFile uuid bytes $> uuid

runConverter :: String -> IO String
runConverter filename = system command $> filename'
    where
        filename' = filename ++ ".txt"
        command   = "I2AA " ++ filename ++ " > " ++ filename'
