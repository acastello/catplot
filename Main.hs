
import Data.Array
import Data.List
import System.Console.Terminal.Size

data RT a 

box :: Array (Int,Int) String -> Int -> Int -> Int -> Int -> Array (Int, Int) String
box arr x y x' y' = arr // ([((x,y), col [0,1] ++ "┌"), ((x',y), "┐" ++ col [])
    , ((x,y'), col [0,1] ++ "└"), ((x',y'), "┘" ++ col [])] ++
    [((j,y''), "─") | j <- [x+1..x'-1], y'' <- [y,y']] ++
    [((x'',j), col [1] ++ "│" ++ col []) | j <- [y+1..y'-1], x'' <- [x,x']]) 
col [] = col [0]
col xs = "\ESC[" ++ foldl1 (\s s' -> s ++ ";" ++ s') (show <$> xs) ++ "m\STX"

-- p :: Array (Int, Int) String -> IO ()
p arr = foldMap putStr $ do
    j <- [ly..uy]
    return $ concat [arr ! (i,j) | i <- [lx..ux]] ++ "\n"
    where
        ((lx, ly), (ux,uy)) = bounds arr
