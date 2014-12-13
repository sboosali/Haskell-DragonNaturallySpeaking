module Main where
import           Data.Conduit.Shell
import qualified System.Environment    as System

-- $ ./map.sh echo 1 2 3
-- 1
-- 2
-- 3

-- $ echo 1 2 3
-- 1 2 3

main = do
 (function:arguments) <- System.getArgs
 map_sh function arguments

map_sh :: String -> [String] -> IO ()
map_sh function arguments = mapM_ execute arguments
 where
 execute argument = run $ proc function [argument]

