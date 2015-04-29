{-# LANGUAGE OverloadedStrings #-}

module Api.UserApi  where

import Snap.PrettySnap
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Types.Player
import Snap.Core
import Snap.Snaplet

data UserApp = UserApp

routes :: [(B.ByteString, Handler b UserApp())]
routes = [
            ("", method POST createUser)
         ]
         
apiInit :: SnapletInit b UserApp
apiInit = makeSnaplet "userApi" "handles users" Nothing $ do
          addRoutes routes
          return UserApp
         
createUser :: Handler b UserApp ()
createUser = do
             body <- readRequestBody 2048
             setStatusCode 201
             setBody $ user body
             where
                  user body = decode body :: Maybe Player