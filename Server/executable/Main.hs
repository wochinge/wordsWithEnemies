{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module which starts the server.
module Main where
import           Control.Lens.TH
import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.Snaplet.Heist
import           Site

-- | Main function which starts the server.
main :: IO ()
main = serveSnaplet defaultConfig initApplication