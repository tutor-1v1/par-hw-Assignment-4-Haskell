https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module Main (main) where

import Lib
import System.IO (IOMode(..), withFile)
import Control.Monad (unless)
import System.Directory (doesPathExist)
import GHC.Conc (numCapabilities)

main :: IO ()
main = do
    let procs = numCapabilities

    exists <- doesPathExist "expected.ppm"
    unless exists $ do
        withFile "expected.ppm" WriteMode $ writeMandel procs runMandelSeq

    withFile "out.ppm" WriteMode $ writeMandel procs runMandelPar
