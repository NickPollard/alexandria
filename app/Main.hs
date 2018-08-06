module Main where

import Data.Functor (($>), (<&>))

import Build
import Package
import Parser

import Options.Applicative

data Cmd = Update | Clean

main :: IO ()
main = runCmd =<< execParser cliOptions
  where
    cliOptions = info commands
      ( fullDesc
     <> progDesc "Manage libraries with Alexandria"
     <> header "test for optparse-applicative" )

commands :: Parser Cmd
commands = subparser (clean <> update)
  where clean = command "clean" (info (pure Clean) (progDesc "Remove all installed depenencies and managed files"))
        update = command "update" (info (pure Update) (progDesc "Remove all installed depenencies and managed files"))

runCmd :: Cmd -> IO ()
runCmd Update = update
runCmd Clean = clean
