{-# LANGUAGE OverloadedStrings #-}

-- | A module which offers more readable method calls.
module Snap.PrettySnap where

import Data.Aeson
import Snap.Core
import Snap.Snaplet

-- | Sets the status code of the server response.
setStatusCode :: MonadSnap m 
              => Int -- ^ Status code of the response
              -> m ()
setStatusCode code = modifyResponse . setResponseCode $ code

--setJSONHeader :: (MonadSnap m) => (Response -> Response) -> m ()
--setJSONHeader = modifyResponse . setHeader "Content-Type" "application/json"

-- | Sets the body a server response.
setResponseBody :: (ToJSON a, MonadSnap m) 
                => a -- ^ object which should be sent. Needs method toJSON to convert object to a JSON
                -> m ()
setResponseBody object = writeLBS . encode $ object