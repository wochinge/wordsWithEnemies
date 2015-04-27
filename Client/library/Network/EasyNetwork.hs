{-# LANGUAGE OverloadedStrings #-}

-- | A modul which offers simplified network commands.
module Network.EasyNetwork (
    put', 
    post', 
    get') where

import Network.Wreq
import Data.ByteString.Lazy
import Data.Aeson
import Control.Lens
import Data.Maybe

-- | Operation for a http-put. Normally used for updating objects.
put' :: ToJSON a
     => String -- ^ the url of the value which should be updated
     -> a -- ^ the new value, which must have the ability to be parsed to a JSON
     -> IO () -- response is not needed
put' url content = do
    put url parsedContent
    return ()
    where parsedContent = toJSON content
    
-- | Operation for a http-post. Normally used for creating values.
post' :: (ToJSON a, FromJSON a) 
      => String -- ^ the url where the new object should be created
      -> a -- ^ the value which should be sent to the server, which must have the ability to be parsed toJSON and back
      -> IO (Maybe a) -- ^ Hopefully the server answers with the created object. Nothing could be returned.
post' url content = do
    response <- post url parsedContent
    return (extractResponse response)
    where parsedContent = toJSON content

-- | Operation for http get. Normally used for retrieving values from the server.
get' :: FromJSON a 
     => String -- ^ url, where the value (which should be read) is stored
     -> IO (Maybe a) -- ^ value
get' url = do
    response <- get url
    return (extractResponse response)
    
-- | Takes a server response, extracts the body and parses it to the needed data type. 
-- | If the value could not be parsed, Nothing is returned.
extractResponse :: FromJSON b 
                => Response ByteString -- ^ response of the server which should be parsed
                -> Maybe b -- ^ parsed data. Data type must have ability to be parsed back from JSON
extractResponse response = decoded
    where 
        decoded = decode body
        body = response ^. responseBody
    