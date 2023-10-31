module Main (main) where
import System.Console.CmdArgs
import Options

main :: IO ()
main = do
    opts <- cmdArgs options
    switchOnOptions opts
