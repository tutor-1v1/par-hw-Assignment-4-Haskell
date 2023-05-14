https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib where

-- In this part of the assignment you will parallelize an already
-- existing program.
--
-- So your first step will be to understand the program (Computes
-- a Mandelbrot Set), looking up the whatever you need to in order
-- to understand how to parallelize it so that it can be faster.
--
-- Correctness is simple: it should do the same thing.
-- But faster.

import System.IO (hPutStrLn, hPutStr, hClose, Handle)
import Text.Printf (printf)

import Control.Monad.Par

import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Control.Monad
import Control.Parallel (pseq, par)

todo :: String -> a
todo = error

{-
 - The constant `c` that we add in z^2 + c
 - in components
 -}
rMax :: Double
rMax = 2.0
rMin :: Double
rMin = -rMax

iMax :: Double
iMax = 1.5
iMin :: Double
iMin = -1.5

{-
 - The resolution - m x n
 -}
resM :: Int
resM = 1920
resN :: Int
resN = 1080

toMandelCoords :: Double -> Double -> (Double, Double)
toMandelCoords x y = (re, im)
    where
    re = rMin + x * ((rMax - rMin) / fromIntegral resM);
    im = iMin + y * ((iMax - iMin) / fromIntegral resN);

{-
 - This is how we could normally run it sequentially, 
 - but it's not as fast as it could be!
 -}
runMandelSeq :: Int -> [[Int]]
runMandelSeq n = do
    -- We do n `div` n here so the benchmark is forced to evaluate
    -- as otherwise we don't use any arguments.
    let resN' = resN - n `div` n

    -- We have map flipped here because it makes it easier to do nested maps
    -- This has the funny side effect of making it look imperative,
    -- where `flip map` looks kinda like `for`
    flip map [0 .. resN' :: Int] $ \y ->
            flip map [0 .. resM - 1 :: Int] $ \x -> do
                let (x', y') = toMandelCoords (fromIntegral x) (fromIntegral y)

                255 * mandelbrot x' y' 0 0 1000


writeMandel :: Int -> (Int -> [[Int]]) -> Handle -> IO ()
writeMandel n runner h = do
    let image = runner n

    hPutStrLn h "P3"
    hPutStrLn h "1920 1080"
    hPutStrLn h "255"

    forM_ image $ \r ->
        forM_ r $ \v ->
            hPutStr h $ printf "%d %d %d\n" v v v


{-
 - Write your own parallel version of `runMandelSeq`! There's lots of
 - ways to go about this, but a common one is rows and columns. Note
 - that this time we return a list of lists instead, as we want to avoid
 - IO.
 -
 - We also have a given parameter `n` which is a "cap" on the amount
 - of parallelism we want, we saw this same thing in the `parMap` we 
 - had on trees with `d` - after some depth `d` we simply ran things 
 - sequentially, as spawning more is actually detrimental after a point.
 -}
runMandelPar :: Int -> [[Int]]
runMandelPar n = runPar $ do
    undefined

{-
 - A simple mandelbrot simulation, giving 0 if it doesn't escape
 - and 1 if it does. `!` is there for strictness, but you don't need to
 - concern yourselves with it.
 -}
mandelbrot :: Double -> Double -> Double -> Double -> Int -> Int
mandelbrot !c_r !c_i !z_r !z_i !iters
    | iters == 0  = 0
    | sqMag > 4   = 1
    | otherwise   = mandelbrot c_r c_i z_r' z_i' $ iters - 1
    where
    (!z_r', !z_i') = (z_r*z_r - z_i*z_i + c_r, 2 * z_r * z_i + c_i)
    sqMag = z_r' * z_r' + z_i' * z_i'


{-
 - A STMQueue is a Queue data structure built on top of a TVar, as with Queues
 - in other languages it should process in FIFO order
 -}

data STMQueue a = TQ (TVar [a])

newSTMQueueIO :: IO (STMQueue a)
newSTMQueueIO = todo "newSTMQueueIO"

{-
 - This function takes an argument a and adds it to our STMQueue 
 -}

putSTMQueue :: STMQueue a -> a -> STM ()
putSTMQueue (TQ tv) a = todo "putSTMQueue"

{-
 - This function takes the next element out of the queue and then returns it. 
 - If there is no element to return it returns `Nothing`
 -}

takeSTMQueue :: STMQueue a -> STM (Maybe a)
takeSTMQueue (TQ tv) = todo "takeSTMQueue"

{-
 - Jose has decided that he has fallen out of love with programming and needs to 
 - return to his roots, selling DVDs. Therefore he has decided to never touch a
 - computer again. Instead he is opening a DVD delivery business called 
 -
 - **Jose's DVD Emporium**
 -
 - This function should: 
 -   1) Initilize all relevent data structures 
 -   2) Make nDrivers threads repersenting our drivers with an abitrary delay (all the threads should have the same delay for the purpose of this assignment)  
 -   3) Make a thread repersenting our receptionist
 -
 - Below you will find a function that is implemented for you 
 - foreverUntil works very similarly to forever but it also takes a 
 - TVar as an argument. It will check this TVar and choose to continue or
 - stop looping
 -
 - Note, you may want to implement this function _after_ you have completed
 - the rest of the functions, or you can implement this function assuming that
 - the rest of the functions work correctly, and then implement those.
 -
 - The strategy is up to you, but either way you'll want to read the
 - descriptions for the rest of the functions before you begin.
 -}

foreverUntil :: TVar Bool -> IO () -> IO ()
foreverUntil l a = do
    closingTime <- readTVarIO l
    if closingTime
    then return ()
    else a >> foreverUntil l a

jose'sDVDEmporium :: Int -> Handle -> Handle -> IO ()
jose'sDVDEmporium nDrivers hIn hOut = todo "jose'sDVDEmporium"

{-
 - This is the function we will pass to our delivery drivers (threads). It takes
 - four arguments: an id number (so we can tell the drivers apart), a delay that
 - repersents how long a delivery will take, a STM Queue of open orders, and a
 - TVar Bool that acts as a lock for stdout so that drivers can print their status
 - when they're done
 -
 - This function should: 
 -   1) Grab the next avaliable order from the queue 
 -   2) Wait the requisite amount of time
 -   3) Print the driver's ID what was delivered to stdout in the format 
 -
 -   `Driver {n} delivered {item}` (dont include the curly brackets, they're there to show you what should be filled in)
 -}

deliver :: Int -> Int -> STMQueue String -> TMVar () -> Handle -> IO ()
deliver dNumber delay orderQueue logLock hOut = todo "deliver"

{-
 - This function repersents our receptionist answering the phone to take your
 - order. It takes our orderQueue and the TVar that signals to foreverUntil 
 - as its arguments.
 -
 - This function should: 
 -   1) Get input from stdin repersenting the order 
 -   2) Check the order against the signal to close the shop 
 -   "5PM" then 
 -      a) if the order reads "5PM" then put the appropriate `Bool` in the
 -         TVar that `foreverUntil` checks for `closingTime`
 -      b) else add the order to the queue
 -}

takeOrders :: STMQueue String -> TVar Bool -> Handle -> IO ()
takeOrders orderQueue signalTVar hIn = todo "takeOrders"
