https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
import Criterion.Main (defaultMain, bench, nf)
import Lib (runMandelSeq, runMandelPar)
import GHC.Conc (numCapabilities)

main :: IO ()
main = do

    defaultMain [ bench "runMandelSeq" $ nf runMandelSeq procs
                , bench "runMandelPar" $ nf runMandelPar procs ]
    where
    procs = numCapabilities
