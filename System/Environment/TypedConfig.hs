-- |
-- Module      : System.Environment.TypedConfig
-- Copyright   : Sebastian Fischer
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This library provides a typed interface to configuration files. It
-- translates configuration files as recognised by John Goerzen's
-- ConfigFile package into command line arguments parsed with Neil
-- Mitchell's cmdargs package. Both packages are reexported for
-- convenience.
module System.Environment.TypedConfig (

  module Data.ConfigFile, module System.Console.CmdArgs,

  getConf, getMultiModeConf

  ) where

import Data.Monoid
import Data.ConfigFile
import Data.Either.Utils

import Control.Applicative

import System.Environment
import System.Console.CmdArgs

getConf :: (Data a, Monoid a) => String -> FilePath -> IO a
getConf = getMultiModeConf [mode mempty]

getMultiModeConf :: (Data a, Monoid a) => [Mode a] -> String -> FilePath -> IO a
getMultiModeConf modes progDesc fileName =
  do file <- forceEither <$> readfile emptyCP fileName
     fileConf <- withArgs (confAsArgs file) (cmdArgs progDesc modes)
     argsConf <- cmdArgs progDesc modes
     return (fileConf `mappend` argsConf)

confAsArgs :: ConfigParser -> [String]
confAsArgs cp = do sec <- "DEFAULT" : sections cp
                   (key,val) <- forceEither $ items cp sec
                   return $ "--" ++ arg sec key val
 where
  arg "DEFAULT" key val =               key ++ "=" ++ val
  arg sec       key val = sec ++ "-" ++ key ++ "=" ++ val
