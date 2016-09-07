module Main where

import Types
import Commands.Options
import Commands.Common
import Options.Applicative



main :: IO ()
main = execParser opts >>= handleCommand
  where
    opts = info (helper <*> parseCommand)
      ( fullDesc
     <> progDesc "Manage tasks in a to-do list"
     <> Options.Applicative.header "thodo - a simple command-line to-do list tool" )
