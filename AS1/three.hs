{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Monad.Writer
import Data.Monoid
import Data.Char
import Data.List

type YicesWriter = Writer T.Text ()

tellLn :: T.Text -> Writer T.Text ()
tellLn w = tell (w `T.snoc` '\n')

showT :: Show a => a -> T.Text
showT a = T.pack $ show a

(<<<) = T.append

d 3  = [1,2]
d 5  = [3,4]
d 7  = [3,4,6]
d 9  = [5,8]
d 11 = [10]
d 12 = [9,11]
d _  = []

j i = "j_" <<< showT i

n = 36
jobs  = [1..12]
jobs' = [5,7,10]

variables :: YicesWriter
variables = do
    tellLn ";; define vars"
    mapM_ define [1..12]
    tellLn ""
    where
        define i = tellLn $ "(define " <<< j i <<< "::int)"

inTime :: YicesWriter
inTime = do
    tellLn ";; all jobs should execute in time"
    mapM_ inTime' jobs
    tellLn ""
    where
        inTime' i = tellLn $ "\t(<= "<<<showT i<<<" "<<<j i<<<") (<= "<<<j i<<<" "<<<showT n<<<")"


dependencies :: YicesWriter
dependencies = do
    tellLn ";; jobs should execute after their dependencies"
    mapM_ dependencies' [(i,k) | i<-jobs, k<-(d i)]
    tellLn ""
    where
        dependencies' (i,k) = tellLn $ "\t(>= (- "<<<j i<<<" "<<<showT i<<<") "<<<j k<<<")"

exclusive :: YicesWriter
exclusive = do
    tellLn ";; Exclusive jobs can not be executed at the same time"
    mapM_ exclusive' [(i,k) | i<-jobs', k<-(delete i jobs')]
    tellLn ""
    where
        exclusive' (i,k) = do
            tell $ "\t (or (>= (- "<<<j i<<<" "<<<showT i<<<") "<<<j k<<<")"
            tellLn $ " (>= (- "<<<j k<<<" "<<<showT k<<<") "<<<j i<<<"))"


showModel :: YicesWriter
showModel = do
    tellLn ";; Show-model"
    mapM_ showJ [1..12]
    where
        showJ i = tellLn $ "(echo \""<<<j i<<< ":  \")"<<<" (eval "<<<j i<<<")"


main = putStr $ T.unpack $ execWriter env
    where
        env = do
            variables
            tellLn "(assert (and"
            inTime
            dependencies
            exclusive
            tellLn "))"
            tellLn "(check)"
            showModel