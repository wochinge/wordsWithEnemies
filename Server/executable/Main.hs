{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap
import           Application

main :: IO ()
main = serveSnaplet defaultConfig initApplication