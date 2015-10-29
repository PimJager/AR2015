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

a i = "a" <<< showT i

variables :: YicesWriter
variables = do
    tellLn "VAR"
    tellLn "c : 0..20;"
    mapM_ (\i -> tellLn $ a i <<< " : 0..100;") [1..7]

init :: YicesWriter
init = do
    tellLn "INIT"
    tellLn "c = 0"
    mapM_ (\i -> tellLn $ "& " <<< a i <<< " = " <<< showT i) [1..7]

trans :: YicesWriter
trans = do
    tellLn "TRANS"
    tellLn "case c < 15 : next(c) = c + 1;"
    tell "     TRUE : next(c) = c"
    mapM_ equal [1..7]
    tellLn "; esac & ("
    mapM_ na [2..6]
    tellLn "FALSE"
    tellLn ")"
    where
        na i = do
            tell $ "(case " <<< a i <<< " < 50 : next("<<< a i <<<") = " <<< a (i-1) <<< " + " <<< a (i+1)  
            mapM_ equal $ filter (/=i) [1..7]
            tellLn ";"
            tell $ "     TRUE : next("<<<a i<<<") = " <<< a i 
            mapM_ equal $ filter (/=i) [1..7]
            tellLn $ "; esac) | "
        equal :: Int -> YicesWriter
        equal j = tell $ " & next("<<<a j<<<") = "<<<a j

spec :: YicesWriter
spec = do
    tellLn "LTLSPEC G !(c >= 15 & ("
    mapM_ na [(i,j) | i<-[1..6], j<-[(i+1)..7]]
    tellLn "FALSE"
    tellLn "))"
    where
        na :: (Int, Int) -> YicesWriter
        na (i,j) = tellLn $ "("<<<a i<<<">=50 & "<<<a j<<<">=50) |"


main = putStr $ T.unpack $ execWriter env
    where
        env = do
            tellLn "MODULE main"
            variables
            Main.init
            trans
            spec