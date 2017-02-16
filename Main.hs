{-# LANGUAGE OverloadedStrings, CPP #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.List 

import System.Posix

import Text.Printf
import Text.Read (readMaybe)

import UI.NCurses

data Env = Env
  { eheight   :: Integer
  , ewidth    :: Integer
  , emin      :: Double
  , emult     :: Double
  , etitle    :: String
  , efoot     :: String
  , ehoriz    :: Bool
  , eborder   :: Bool
  , eref      :: Bool
  , escalef   :: ScaleF
  , edata     :: [Double]
  , eframe    :: Window
  , egraph    :: Window
  , estdin    :: Fd
  } 

defaultEnv :: Env
defaultEnv = Env 0 0 0 1 "" "" True True True NoScale [] undefined undefined 0

data ScaleF = NoScale | LogScale

type RT = StateT Env Curses

runRT :: RT a -> IO a
runRT op = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    win <- newWindow 1 1 0 0
    swin <- subWindow win 1 1 0 0
    fd <- liftIO $ dup 0
    evalStateT (updateRT >> adjustWin >> op) 
        defaultEnv { eframe = win, egraph = swin, estdin = fd }

updateRT :: RT ()
updateRT = do
    e <- get
    (h,w) <- lift screenSize
    put e { eheight = h, ewidth = w } 

margins :: RT (Integer, Integer, Integer)
margins = do
    e <- get
    let up = if null $ etitle e then 0 else 1
        dow = if null $ efoot e then 0 else 1
        lef = if eref e then 5 else 0
    return (up,dow,lef)

adjustScale :: Integer -> Stats -> RT ()
adjustScale h' s = do
    e @ Env { emin = mi, emult = mu' } <- get
    let h = fromIntegral h'
        mu = mu' * h
        ltres = mi + 0.2*mu
        utres = mi + 0.8*mu
        iltres = mi + 0.4*mu
        iutres = mi + 0.6*mu 
    if or [s_min s < mi, s_max s > mi+mu ,s_q1 s < ltres, s_q4 s > utres] then do
        -- liftIO $ putStrLn "expanding"
        let r = s_max s - s_min s
            f = 0.2
            mi = s_min s - f * r
            mu = r * (1 + 2*f) / h
        put e { emin = mi, emult = mu }
    -- TODO: fix unscattered samples
    else if or [] then do
        -- liftIO $ putStrLn $ "shrinking"
        return ()
    else do
        -- liftIO $ putStrLn $ "nuffin"
        return ()
    return ()

adjustData :: Integer -> Integer -> RT [Integer]
adjustData h w = do
    e <- get
    let takes = take (fromIntegral w `quot` 2) (edata e)
        f x = (x - emin e) / emult e 
    put e { edata = takes }
    return $ reverse $ (h -) . round . f <$> takes

adjustWin :: RT [Integer]
adjustWin = do
    e @ Env { eheight = h, ewidth = w } <- get
    (u,d,l) <- margins
    let b = if eborder e then 1 else 0
        winx = u
        winy = l
        winh = h - u - d
        winw = w - l
        subx = u + b
        suby = l + b
        subh = winh - b
        subw = winw - b
    doresize <- lift $ updateWindow (eframe e) $ do
        (oldh, oldw) <- windowSize
        if oldh /= winh || oldw /= winw then do
            resizeWindow winh winw
            moveWindow winx winy
            when (eborder e) $ drawBox Nothing Nothing
            return True
        else
            return False
    lift $ updateWindow (egraph e) $ do
        when doresize $ do
            resizeWindow (winh - 2*b) (winw - 2*b)
            moveWindow (u+b) (l+b)
        clearLines [0..winh -2*b-1]

    adjustScale (subh - 2) (statLimits $ edata e)
    adjustData (subh - 2) (subw - 2)

drawGraph :: [Integer] -> RT ()
drawGraph dat = do
    e <- get
    let b = if eborder e then 1 else 0
        f y0 (x,y1) = (Just y1, do
            forM_ y0 $ \y -> drawPilon y y1 (x-1)
            moveCursor y1 x
            drawGlyph glyphBlock
            )
    lift $ do
        updateWindow (egraph e) $ sequence_ $ snd $ mapAccumL f Nothing $ zip [b,b+2..] dat
        render

drawComplete :: RT ()
drawComplete = do
    updateRT
    dat <- adjustWin 
    drawGraph dat

getDouble :: RT ()
getDouble = do
    maybed <- readMaybe <$> getLine'
    case maybed of 
        Nothing -> getDouble
        Just d -> do
            modify (\e -> e { edata = d : edata e })

test xs = runRT $ do
    e <- get
    put e { edata = xs }
    updateRT 
    adjustWin 

main = runRT $ forever $ do
    getDouble
    drawComplete 

main' = runCurses $ do
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

draw :: Window -> Integer -> [Integer] -> Curses ()
draw win x dat = updateWindow win $ sequence_ $ snd 
                 $ mapAccumL f Nothing $ zip [x,x+2..] dat where
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
statLimits [] = Stats 0 1 0 0 0 0 0 0 
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

ff :: Double -> String
ff d
  | abs d >= 10000 = printf "%5e" d
  | otherwise = printf "%d" d

getLine' :: RT String
getLine' = do
    r <- liftIO $ tryIO getLine
    either f return r where
        f _ = do
            e @ Env { eframe = win } <- get
            lift $ do
                updateWindow win $ do
                    (h,w) <- windowSize
                    let msg = "clossing in 1 second"
                    moveCursor (h `quot` 2) 
                               ((w - (fromIntegral $ length msg)) `quot` 2)
                    drawString msg
                render
            liftIO $ threadDelay 1000000
            mzero

tryIO :: IO a -> IO (Either IOException a)
tryIO = try 
