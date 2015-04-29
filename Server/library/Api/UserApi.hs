{-# LANGUAGE OverloadedStrings #-}

module Api.UserApi  where

import Snap.PrettySnap
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Types.Player
import Snap

data UserApp = UserApp

routes :: [(B.ByteString, Handler UserApp UserApp())]
routes = [
            ("", method POST createUser)
         ]
         
apiInit :: SnapletInit UserApp UserApp
apiInit = makeSnaplet "userApi" "handles users" Nothing $ do
          addRoutes routes
          return UserApp
         
createUser :: Handler UserApp UserApp ()
createUser = do
             body <- readRequestBody 2048
             setStatusCode 201
             setBody $ user body
             where
                  user body = decode body :: Maybe Player