
import Data.Array
import Data.List
import System.Console.Terminal.Size

box :: Array (Int,Int) String -> Int -> Int -> Int -> Int -> Array (Int, Int) String
box arr x y x' y' = arr // ([((x,y), col [0,1] ++ "┌"), ((x',y), "┐" ++ col [])
    , ((x,y'), col [0,1] ++ "└"), ((x',y'), "┘" ++ col [])])
col [] = col [0]
col xs = "\ESC[" ++ foldl1 (\s s' -> s ++ ";" ++ s') (show <$> xs) ++ "m\STX"

-- p :: Array (Int, Int) String -> IO ()
-- p = sequence
