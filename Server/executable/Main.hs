{-# LANGUAGE OverloadedStrings #-}

-- | Module which starts the server.
module Main where

import           Snap
import           Site

-- | Main function which starts the server.
main :: IO ()
main = serveSnaplet defaultConfig initApplication