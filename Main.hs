{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Data.Array
import Data.List 

import System.Console.Terminal.Size
import System.Console.Terminfo

import Text.Printf
import Text.Read (readMaybe)

data Term = Term
    { mv    :: Int -> Int -> RT ()
    , dims  :: RT (Int,Int)
    , bold  :: RT ()
    , dim   :: RT ()
    , reset :: RT ()
    , clear :: Int -> RT ()
    , buff  :: String
    }

type RT = StateT Term IO

flush :: RT ()
flush = do
    t <- get
    liftIO $ putStr (buff t)
    put t { buff = "" }

chain :: String -> RT ()
chain s = do
    t @ Term { buff = buff } <- get
    put t { buff = buff ++ s }

pc :: Char -> RT ()
pc = chain . pure

runRT :: RT a -> IO a
runRT op = do
    term <- setupTermFromEnv
    let cap c = maybe undefined chain (getCapability term c)
        dims = liftIO $ do
            w <- maybe (error "couldn't get terminal size") id <$> size
            return (height w, width w)
        act output y x = chain (output (Point y x))
        mv = maybe (error "moving the cursor is not supported in this terminal") 
                act (getCapability term cursorAddress)
        clear n = mv n 0 >> cap clearEOL
    evalStateT op (Term mv dims (cap boldOn) (cap dimOn) (cap allAttributesOff)
        clear "") where

main :: IO ()
main = void $ runRT $ do
    t <- get
    (h,w) <- dims t
    mainLoop h w []

mainLoop :: Int -> Int -> [Double] -> RT [Double]
mainLoop h w zs = do
    t @ Term { clear = clear } <- get
    drawComplete (h-1) (w-1) (reverse $ adjust (h-3) $ take (w-3) zs)
    let readSure = do
        str <- liftIO getLine
        case readMaybe str of
            Nothing -> readSure
            Just x -> return x
    flush
    z <- log <$> readSure
    (h', w') <- dims t
    if h' == h && w == w' then
        clearList (h-2) 1 $ (reverse $ adjust (h-3) $ take (w-3) zs)
    else do
        mapM_ clear [0..h']
    mainLoop h' w' (z:zs)

drawComplete :: Int -> Int -> [Int] -> RT ()
drawComplete y x zs = do
    t <- get
    -- forM_ [0..y] (clear t)
    -- drawLines 0 1 y (x-1)
    -- drawBox 0 0 y x
    drawList (y-1) 1 $ zs

adjustFactors :: Int -> [Double] -> (Double, Double)
adjustFactors n xs = (a, b) where
    a = fromIntegral n / range
    b = - dif * a
    mean = sum xs / fromIntegral (length xs)
    mdev = maximum $ abs <$> subtract mean <$> xs
    dif = mean - mdev
    range = 2 * mdev

reverseAdj :: (Double, Double) -> Double -> Double
reverseAdj (a,b) x = (x-b)/a

adjust :: Int -> [Double] -> [Int]
adjust n xs = fmap (round . (* (fromIntegral n/range)) . (subtract min)) xs where
    mean = sum xs / fromIntegral (length xs)
    mdev = maximum $ abs <$> (subtract mean) <$> xs
    min = mean - mdev
    range = 2*mdev

pfloat :: Double -> String
pfloat d
  | d == 0                        = "0"
  | abs d >= 10000.0 || abs d < 0.00999   = printf "%0.2e" d
  | otherwise = printf "%.*f" (5 - (1 + floor (logBase 10 (abs d))) :: Int) d

drawList :: Int -> Int -> [Int] -> RT ()
drawList y x zs = do
    t <- get
    let f j n = when (n >= 0) $ do
        (mv t) (y-n) j
        pc '×'
    sequence_ $ zipWith f [x..] zs

clearList :: Int -> Int -> [Int] -> RT ()
clearList y x zs = do
    t <- get
    let f j n = when (n >= 0) $ do
        (mv t) (y-n) j
        pc ' '
    sequence_ $ zipWith f [x..] zs

drawBox :: Int -> Int -> Int -> Int -> RT ()
drawBox yo xo yf xf = do
    t @ Term { mv = mv } <- get
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
    t <- get
    forM_ [xo..xf] $ \j -> do
        (mv t) y j
        pc '─'

drawLines :: Int -> Int -> Int -> Int -> RT ()
drawLines yo xo yf xf = do
    t <- get
    dim t
    forM_ [yf-(step-1)-step*y | y <- if yr < (step+2) then [] else [0..yr`quot`step]] $ \i -> do
        drawLine i xo xf
    reset t
    where
        yr = yf-yo
        step = 3
