{-# LANGUAGE OverloadedStrings #-}

-- | A module which offers more readable method calls.
module Snap.PrettySnap 
( setStatusCode
, setBody
, decodeBody
) where

import 			 Data.Aeson
import 			 Data.Maybe
import 			 Snap.Core
import 			 Snap.Snaplet
import qualified Data.ByteString.Lazy as B

-- | Sets the status code of the server response.
setStatusCode :: MonadSnap m 
              => Int -- ^ Status code of the response
              -> m ()
setStatusCode code = modifyResponse . setResponseCode $ code

-- | Sets the content-type of the answer to application/json
setJSONHeader :: MonadSnap m => m ()
setJSONHeader = modifyResponse header
    where header = setHeader "Content-Type" "application/json"
    
-- | Sets the body of a server response.
setBody :: (ToJSON a, MonadSnap m) 
        => a -- ^ object which should be sent. Needs method toJSON to convert object to a JSON
        -> m ()
setBody object = do
    setJSONHeader
    writeLBS . encode $ object

-- | Decodes JSON body back to data.
decodeBody :: FromJSON a 
           => B.ByteString -- ^ body 
           -> a            -- ^ decoded data
decodeBody body = fromJust $ decode body