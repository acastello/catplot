{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Data.List 

import Text.Printf
import Text.Read (readMaybe)

import UI.NCurses

drawComplete :: Window -> [Integer] -> Curses ()
drawComplete w xs = do
    updateWindow w $ sequence $ zipWith f [0..] xs
        where
            f n x = do

