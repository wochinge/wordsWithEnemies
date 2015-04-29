{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Lens.TH
import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.Snaplet.Heist
import           Application

main :: IO ()
main = serveSnaplet defaultConfig initApplication