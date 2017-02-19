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
  , pointch   :: Glyph
  , lineattrs :: [Attribute]
  , horizch   :: Glyph
  } 

defaultEnv :: Env
defaultEnv = Env 0 0 0 1 "" "" True True True LogScale [] undefined undefined
                  (Glyph '\9724' [])
                  []
                  glyphLineH { glyphAttributes = [AttributeDim] }

data ScaleF = NoScale | LogScale

adj :: ScaleF -> Double -> Double
adj NoScale = id
adj LogScale = logBase 10

rev :: ScaleF -> Double -> Double
rev NoScale = id
rev LogScale = (10**)

showrev :: Integer -> RT String
showrev x = do
    e <- get
    return $ ff $ rev (escalef e) $ ((fromIntegral x)) * (emult e) + (emin e)

type RT = StateT Env Curses

runRT :: RT a -> IO a
runRT op = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    win <- newWindow 1 1 0 0
    swin <- subWindow win 1 1 0 0
    fd <- liftIO $ dup 0
    evalStateT (updateRT >> adjustWin >> op) 
        defaultEnv { eframe = win, egraph = swin }

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
        lef = if eref e then 8 else 0
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
        suby = u + b
        subx = l + b
        subh = winh - 2*b
        subw = winw - 2*b

    doresize <- lift $ updateWindow (eframe e) $ do
        (oldh, oldw) <- windowSize
        if oldh /= winh || oldw /= winw then do
            resizeWindow winh winw
            moveWindow winx winy
            return True
        else
            return False

    when doresize $ lift $ updateWindow (egraph e) $ do
        resizeWindow subh subw
        moveWindow suby subx

    let (mi,mu) = (emin e, emult e)

    adjustScale (subh - 2) (statLimits $ edata e)
    dat <- adjustData (subh - 2) (subw - 2)

    e <- get

    let reref = (eref e) && ((emult e /= mu) || (emin e /= mi))
    when reref $ drawRef (subh-1) suby

    when (reref || (doresize && eborder e)) $ lift $ updateWindow (eframe e) 
            $ drawBox Nothing Nothing

    lift $ updateWindow (egraph e) $ do
        clearLines [0..subh-1]
    when (ehoriz e) $ drawHorizontal (subh-1) (subw-1)


    return dat

drawRef :: Integer -> Integer -> RT ()
drawRef h y0 = do
    e <- get
    let ys = (y0+) <$> filter (\y -> (h - y) `mod` 4 == 0) [0..h]
    -- liftIO $ print ys
    w <- lift defaultWindow
    forM_ ys $ \y -> do
        str <- showrev (h + y0 - y)
        lift $ do
            updateWindow w $ do
                moveCursor y 0
                drawString str

drawHorizontal :: Integer -> Integer -> RT ()
drawHorizontal h w = do
    e <- get
    let ys = filter (\y -> (h-y) `mod` 4 == 0) [0..h]
    lift $ updateWindow (egraph e) $ forM_ ys $ \y -> do
        moveCursor y 0
        drawLineH (Just glyphLineH { glyphAttributes = [AttributeDim] }) w

drawGraph :: [Integer] -> RT ()
drawGraph dat = do
    e <- get
    let b = if eborder e then 1 else 0
        f y0 (x,y1) = (Just y1, do
            forM_ y0 $ \y -> drawPilon y y1 (x-1) (lineattrs e)
            moveCursor y1 x
            drawGlyph (pointch e)
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
            modify (\e -> e { edata = adj (escalef e) d : edata e })

test xs = runRT $ do
    e <- get
    put e { edata = xs }
    updateRT 
    adjustWin 

main = runRT $ forever $ do
    getDouble
    drawComplete 

drawPilon y0 y1 x as =
    if y0 == y1 then do
        moveCursor y0 x 
        drawGlyph glyphLineH { glyphAttributes = as }
    else if y0 > y1 then do
        moveCursor y0 x
        drawGlyph glyphCornerLR { glyphAttributes = as }
        forM_ [y1+1..y0-1] $ \y -> do
            moveCursor y x
            drawGlyph glyphLineV { glyphAttributes = as }
        moveCursor y1 x
        drawGlyph glyphCornerUL { glyphAttributes = as }
    else do
        moveCursor y0 x
        drawGlyph glyphCornerUR { glyphAttributes = as }
        forM_ [y0+1..y1-1] $ \y -> do
            moveCursor y x
            drawGlyph glyphLineV { glyphAttributes = as }
        moveCursor y1 x
        drawGlyph glyphCornerLL { glyphAttributes = as }

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
ff d = printf "%8s" (printf f d :: String) where
    a = abs d
    f 
      | a > 9999.9 || a < 0.01 = "% .2e"
      | a > 999.99    = "% .1f"
      | a > 9.999     = "% .2f"
      | a > 0.99      = "% .3f"
      | a > 0.09999   = "% .4f"
      | otherwise     = "% .5f"

flog :: Double -> String
flog d = printf "%8s" (printf "% .2e" d :: String) 

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
