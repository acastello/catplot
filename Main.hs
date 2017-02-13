{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Data.List 

import Text.Printf
import Text.Read (readMaybe)

import UI.NCurses

data Env = Env

drawComplete :: Window -> [Integer] -> Curses ()
drawComplete w xs = do
    undefined

main = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    dft <- defaultWindow
    draw <- drawer
    let read' = do
        d <- readMaybe <$> liftIO getLine
        case d of 
            Nothing -> read'
            Just x -> return x
    let f xs = do
        when (xs /= []) $
            draw xs
        x <- log <$> read'
        f (x:xs)
    f []


drawer :: Curses ([Double] -> Curses ())
drawer = do
    win <- newWindow 1 1 0 0
    return $ \xs -> do
        (h,w) <- screenSize
        updateWindow win $ do
            let upad = 1
                lpad = 7
                rpad = 0
            resizeWindow (h-upad) (w-lpad-rpad)
            clearLines [0..h-upad-1]
            moveWindow upad lpad
            drawBox Nothing Nothing
            let h' = h - upad - 2
                n = fromIntegral $ w - lpad - rpad - 2
                ys = reverse $ take (n`quot`2) xs
                (f1,_) = adjf (statLimits ys) h'
            draw' h' (f1 <$> ys)
        render
  where
        draw' n xs = sequence $ snd $ mapAccumL f Nothing $ zip [1,3..] ((n -) <$> xs)
        f y0 (x,y1) = (Just y1, do
            forM_ y0 $ \y -> drawPilon y y1 (x-1)
            moveCursor y1 x
            drawGlyph glyphBlock)


drawPilon y0 y1 x =
    if y0 == y1 then do
        moveCursor y0 x 
        drawGlyph glyphLineH
    else if y0 > y1 then do
        moveCursor y0 x
        drawGlyph glyphCornerLR
        forM_ [y1+1..y0-1] $ \y -> do
            moveCursor y x
            drawGlyph glyphLineV
        moveCursor y1 x
        drawGlyph glyphCornerUL
    else do
        moveCursor y0 x
        drawGlyph glyphCornerUR
        forM_ [y0+1..y1-1] $ \y -> do
            moveCursor y x
            drawGlyph glyphLineV
        moveCursor y1 x
        drawGlyph glyphCornerLL

data Stats = Stats
    { s_min   :: Double
    , s_max   :: Double
    , s_q1    :: Double
    , s_q2    :: Double
    , s_q3    :: Double
    , s_q4    :: Double
    , s_mean  :: Double
    , s_med   :: Double
    } deriving Show

adjf :: Stats -> Integer -> (Double -> Integer, Integer -> Double)
adjf s h = ( \d -> round $ (d-mi) * fromIntegral h / r
           , \i -> fromIntegral i * r / fromIntegral h + mi) where
    m = s_med s
    d4 = s_q4 s - m
    ud = 2 * (s_max s - m) - d4
    d1 = m - s_q1 s
    ld = 2 * (m - s_min s) + d1
    d = max ud ld
    r = 2 * d
    mi = m - d

--     adjusted at margins, biased against scattered lines
--     d4 = if s_q4 s < s_mean s then 0 else s_q4 s - s_mean s
--     ud = s_max s - s_mean s + d4
--     ul = s_max s + ud
--     d1 = if s_q1 s > s_mean s then 0 else s_q1 s - s_mean s
--     ld = s_min s - s_mean s + d1
--     ll = s_min s + ld
--     r = ul - ll
--     mi = ll

statLimits :: [Double] -> Stats
statLimits xs = Stats mi ma q1 q2 q3 q4 me md where
    l = length xs - 1
    me = (sum xs) / fromIntegral l
    md = let (q,r) = quotRem l 2 in (xs !! q + xs !! (q+r)) / 2
    oxs = sort xs
    mi = head oxs
    ma = last oxs
    [q1,q2,q3,q4] = (\i -> xs !! (l*i`quot`5)) <$> [1,2,3,4]

xs :: [Double]
xs = [0,0.5,0.5,1,1.5,2.5,4,6.5,10.5,17]

ys :: [Double]
ys = [3.5, 3.7, 4.0, 3.6, 3.6, 12, 90, 20, 4.5, 9]

write' :: Either [Glyph] String -> IO ()
write' str = runCurses $ do
    dft <- defaultWindow
    updateWindow dft $ do
        moveCursor 1 4
        either (mapM_ drawGlyph) drawString str
    render
    void $ getEvent dft Nothing
