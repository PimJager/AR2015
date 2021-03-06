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

numA = 7
n = 8

a i j = "a_" <<< showT i <<< "_" <<< showT j

variables :: YicesWriter
variables = do
    tellLn ";; define all vars"
    mapM_ (\i-> mapM_ (defineA i) [1..n]) [1..numA]
    tellLn ""
    where
        defineA i j = tellLn $ "(define " <<< a i j <<< "::int)"

init :: YicesWriter
init = do
    tellLn ";; Initial values (TIMESTEP 1)"
    mapM_ vals [1..numA]
    tellLn ""
    where
        vals :: Int -> YicesWriter
        vals i = tellLn $ "(= "<<<a i 1<<<" "<<<showT i<<<")"


trans :: YicesWriter
trans = do
    tellLn ";; transformation for each timestep:"
    tellLn ""
    mapM_ step [2..n]
    tellLn ""
    where
        step j      = do 
            tellLn $ ";;---TIMESTEP: " <<< showT j
            tellLn $ "(or "
            mapM_ ((flip(stepA)) j) [2..(numA-1)]
            tellLn $ ")"
        stepA i j   = do
            tellLn $ "\t (and (= "<<<a i j<<<" (+ "<<<a (i-1) (j-1)<<<" "<<<a (i+1) (j-1)<<<"))"
            tell "\t\t" >> (mapM_ ((flip(same) j)) $ filter (/=i) [1..numA]) >> tellLn ""
            tellLn $ "\t )"
        same :: Int -> Int -> YicesWriter
        same i j   = tell $ "(= "<<<a i j <<<" "<<<a i (j-1)<<<") "

spec :: YicesWriter
spec = do
    tellLn ";; at least two a_ij should have values > 50"
    tellLn "(or "
    mapM_ check [(a1, a2) | a1<-[1..(numA-1)], a2<-[(a1+1)..numA]]
    tellLn ")"
    tellLn ""
        where
            check (i1, i2) = tellLn $ "(and (>= "<<< a i1 n<<<" 50) (>= "<<<a i2 n<<<" 50))"

showModel :: YicesWriter
showModel = do
    tellLn ";; show all vars"
    mapM_ (\i-> mapM_ (showA i) [1..n]) [1..numA]
    tellLn ""
    where
        showA i j = tellLn $ "(echo \""<<< a i j<<< ":  \")"<<<" (eval "<<<a i j<<<")"

main = putStr $ T.unpack $ execWriter env
    where
        env = do
            variables
            tellLn "(assert (and"
            Main.init
            trans
            spec
            tellLn "))"
            tellLn "(check)"
            showModel
