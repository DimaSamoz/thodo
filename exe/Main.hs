module Main where

import Types
import Commands.Parser
import Commands.Common
import Options.Applicative

greet :: Command -> IO ()
greet c = print c



main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> parseCommand)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> Options.Applicative.header "hello - a test for optparse-applicative" )
