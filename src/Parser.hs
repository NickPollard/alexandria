module Parser where

import Options.Applicative

quiet :: Parser Bool
quiet = switch ( long "quiet" <> short 'q' <> help "Whether to be quiet" )
