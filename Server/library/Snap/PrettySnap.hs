{-# LANGUAGE OverloadedStrings #-}

-- | A module which offers more readable method calls for the snap framework.
module Snap.PrettySnap 
( setStatusCode
, setBody
, getIdParam
, getJSONBody
) where

import 			 Data.Aeson (encode, decode, FromJSON, ToJSON)
import 			 Data.Maybe (fromJust)
import 			 Snap.Core (modifyResponse, setResponseCode, writeLBS, setHeader, getParam, readRequestBody, MonadSnap)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B_Char (pack, unpack)

-- | Sets the status code of the server response.
setStatusCode :: MonadSnap m 
              => Int -- ^ Status code of the response
              -> m ()
setStatusCode = modifyResponse . setResponseCode

-- | Sets the content-type of the answer to application/json
setJSONHeader :: MonadSnap m => m ()
setJSONHeader = modifyResponse header
    where header = setHeader "Content-Type" "application/json"
    
-- | Sets the body of a server response.
setBody :: (ToJSON a, MonadSnap m) 
        => a -- ^ object which should be sent. Needs method toJSON to convert object to a JSON
        -> m ()
setBody bodyObject = do
    setJSONHeader
    writeLBS . encode $ bodyObject

-- | Decodes JSON body back to data.
decodeBody :: FromJSON a 
           => B.ByteString -- ^ body 
           -> a            -- ^ decoded data
decodeBody = fromJust . decode

-- | Gets an id from the url params.
getIdParam :: MonadSnap m 
		   => String    -- ^ url param identifier
		   -> m Integer -- ^ id
getIdParam param = do
	bytestring <- getParam $ B_Char.pack param
	return $ read $ B_Char.unpack $ fromJust bytestring

-- | Gets a json object from a request body.
getJSONBody :: (MonadSnap m, FromJSON a) 
			=> m a --  ^ decoded JSON body
getJSONBody = do
	body <- readRequestBody 2048
	return $ decodeBody body