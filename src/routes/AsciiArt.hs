{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Routes.AsciiArt ( asciiArt
                       ) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.Functor                 (($>))
import           Data.ByteString        as BS (ByteString, writeFile)
import           Data.Text                    (Text, pack, unpack)
import           GHC.Generics

import           Data.Aeson            hiding (json)                        
import           Data.GUID                    (genString)
import           Network.Curl.Download        (openURI)
import           System.Process               (system)
import           Web.Spock

import           Config

data AsciiArtRequest = InvalidRequest
                     | AsciiArtRequest { urlParam :: Text
                                       } deriving(Show)

data AsciiArtResponse = AsciiArtResponse { success :: Bool
                                         , art     :: Maybe Text
                                         } deriving(Generic, Show)

instance ToJSON   AsciiArtResponse


asciiArt :: ApiAction ()
asciiArt = getUrl >>= loadFile >>= sendArt
    where
        getUrl = unpack . urlParam <$> getParams
        
        sendArt (Just bytes) = do
            aArt <- pack <$> (liftIO $ processImage bytes)
            sendJSON True (Just aArt)
        sendArt Nothing      =
            sendJSON False Nothing

        processImage bytes = writeTempFile bytes >>= runConverter >>= readFile            

        sendJSON success' art' = json $ AsciiArtResponse { success = success', art = art' }



-- PARSE REQUEST PARAMS


getParams :: ApiAction AsciiArtRequest
getParams = buildRequest <$> param "url"
    where
        buildRequest (Just url) = AsciiArtRequest url
        buildRequest Nothing    = InvalidRequest



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
        command   = "ascii-art-exe " ++ filename ++ " > " ++ filename'
