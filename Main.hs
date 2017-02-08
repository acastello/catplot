{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Array
import Data.List 

import System.Console.Terminal.Size
import System.Console.Terminfo


data Term = Term
    { mv    :: Int -> Int -> RT ()
    , dims  :: RT (Int,Int)
    , bold  :: RT ()
    , dim   :: RT ()
    , reset :: RT ()
    }

type RT = ReaderT Term IO

pc :: Char -> RT ()
pc = liftIO . putChar

runRT :: RT () -> IO ()
runRT op = do
    term <- setupTermFromEnv
    let cap c = maybe undefined (liftIO . runTermOutput term) (getCapability term c)
        dims = liftIO $ do
            w <- maybe (error "couldn't get terminal size") id <$> size
            return (height w, width w)
        act output y x = liftIO $ runTermOutput term (output (Point y x))
        mv = maybe (error "moving the cursor is not supported in this terminal") 
                act (getCapability term cursorAddress)
    runReaderT op (Term mv dims (cap boldOn) (cap dimOn) (cap allAttributesOff)) where

drawComplete :: Int -> Int -> [Int] -> RT ()
drawComplete y x zs = do
    drawLines 0 0 y x
    drawBox 0 0 y x
    drawList (y-1) (x+1) zs

drawList :: Int -> Int -> [Int] -> RT ()
drawList y x zs = do
    t <- ask
    let f j n = do
        (mv t) (y-n) j
        pc '∙'
    sequence_ $ zipWith f [x..] zs

drawBox :: Int -> Int -> Int -> Int -> RT ()
drawBox yo xo yf xf = do
    t @ Term { mv = mv } <- ask
    bold t
    mv yo xo
    liftIO $ putChar '┌'
    mv yo xf
    liftIO $ putChar '┐'
    mv yf xo
    liftIO $ putChar '└'
    mv yf xf
    liftIO $ putChar '┘'
    forM_ [xo+1..xf-1] $ \j -> do
        mv yo j
        pc '─'
        mv yf j
        pc '─'
    forM_ [yo+1..yf-1] $ \i -> do
        mv i xo
        pc '│'
        mv i xf
        pc '│'
    reset t

drawLine :: Int -> Int -> Int -> RT ()
drawLine y xo xf = do
    t <- ask
    forM_ [xo..xf] $ \j -> do
        (mv t) y j
        pc '─'

drawLines :: Int -> Int -> Int -> Int -> RT ()
drawLines yo xo yf xf = do
    t <- ask
    dim t
    forM_ [yf-(step-1)-step*y | y <- if yr < (step+2) then [] else [0..yr`quot`step]] $ \i -> do
        drawLine i xo xf
    reset t
    where
        yr = yf-yo
        step = 3
