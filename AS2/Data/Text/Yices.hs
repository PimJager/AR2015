{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Yices where

import Prelude hiding (print, and, or)
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T

(<<<) = T.append

class Arith x where
    lit  :: Show a => a -> x a
    vN   :: Num a => T.Text -> x a
    vB   :: T.Text -> x Bool
    (+:) :: Num a => x a -> x a -> x a
    (-:) :: Num a => x a -> x a -> x a
    (*:) :: Num a => x a -> x a -> x a
    (/:) :: Num a => x a -> x a -> x a
    (^:) :: Num a => x a -> x a -> x a
    (%:) :: Num a => x a -> x a -> x a
    neg  :: Num a => x a -> x a
    abs  :: Num a => x a -> x a
class Compare x where
    (=:)    :: x a -> x a -> x Bool
    (/=:)   :: x a -> x a -> x Bool
    (>:)    :: x a -> x a -> x Bool 
    (>=:)   :: x a -> x a -> x Bool 
    (<:)    :: x a -> x a -> x Bool 
    (<=:)   :: x a -> x a -> x Bool 
    not     :: x Bool -> x Bool
class AndOr x where
    and :: [x Bool] -> x Bool
    or  :: [x Bool] -> x Bool
class Ite x where
    ite :: x Bool -> x a -> x a -> x a
class (Arith a, Compare a, AndOr a, Ite a) => Expr a

data Program x = Program {vars :: [Var], program :: [x Bool], results :: [T.Text]}
data Var = I T.Text | B T.Text

type Print = State PrinterState
data PrinterState = PS {cont :: T.Text, ident :: Int} deriving (Show)
emptyPS = PS {cont = T.empty, ident = 0}

instance (Num a, Show a) => Num (Print a) where
    --we really only care about fromInteger
    fromInteger = lit . fromInteger
    negate      = neg
    abs         = Data.Text.Yices.abs
    signum      = undefined --scrw this
    (+)         = (+:) 
    (-)         = (-:)
    (*)         = (*:)    
instance (Fractional a, Num a, Show a) => Fractional (Print a) where
    fromRational    = lit . fromRational
    (/)             = (/:)

print :: T.Text -> Print ()
print a = modify (\s -> s{cont = cont s <<< a})

printTabs :: Print()
printTabs = modify (\s -> s{cont = cont s <<< tabs (ident s)})
    where tabs n = (T.pack $ replicate n '\t')

printBinOp :: T.Text -> Print a -> Print a -> Print ()
printBinOp op a b = print "(" >> print op >> print " " >> a >> print " " >> b >> print ")"
instance Arith Print where
    lit a       = (print $ T.pack $ show a) >> return a
    vN k        = print k >> return 0
    vB k        = print k >> return False
    (+:) a b    = printBinOp "+" a b >> return 0
    (-:) a b    = printBinOp "-" a b >> return 0 
    (*:) a b    = printBinOp "*" a b >> return 0  
    (/:) a b    = printBinOp "/" a b >> return 0  
    (^:) a b    = printBinOp "^" a b >> return 0 
    (%:) a b    = printBinOp "mod" a b >> return 0
    neg  a      = print "(- " >> a >> print ")" >> return 0
    abs  a      = print "(abs " >> a >> print ")" >> return 0 
instance Compare Print where
    (=:)  a b   = printBinOp ("=") a b >> return False
    (/=:) a b   = printBinOp ("/=") a b >> return False
    (>:)  a b   = printBinOp (">") a b >> return False 
    (>=:) a b   = printBinOp (">=") a b >> return False 
    (<:)  a b   = printBinOp ("<") a b >> return False 
    (<=:) a b   = printBinOp ("<=") a b >> return False 
    not   a     = print "(not " >> a >> print ")" >> return False
instance AndOr Print where
    and xs      = printCommutative "and" xs >> return False
    or xs      = printCommutative "or" xs >> return False
printCommutative :: T.Text -> [Print a] -> Print ()
printCommutative op xs = do
        print "(" >> print op >> print "\n"
        modify (\s -> s{ident = ident s + 1})
        mapM_ (\x -> printTabs >> x >> print "\n") xs
        modify (\s -> s{ident = ident s - 1})
        printTabs >> print ")"
instance Ite Print where
    ite c t e   = do 
        print "(ite " >> print "\n"
        modify (\s -> s{ident = ident s + 1})
        printTabs >> c >> print "\n"
        printTabs 
        ret <- t 
        print "\n"
        printTabs >> e >> print "\n"
        modify (\s -> s{ident = ident s - 1})
        printTabs >> print ")"
        return ret
instance Expr Print



printExpr       = putStr . T.unpack . showExpr
showExpr expr   = cont $ execState expr emptyPS

showVars :: [Var] -> T.Text
showVars = showVars' T.empty
showVars' :: T.Text -> [Var] -> T.Text
showVars' c []       = c
showVars' c (v:vs) = showVars' (c <<< "(define "<<<getKey v<<<"::"<<<getType v<<<")\n") vs

showModel :: [T.Text] -> T.Text
showModel vs = "(check)\n" <<< showModel' T.empty vs
showModel' :: T.Text -> [T.Text] -> T.Text
showModel' c []     = c
showModel' c (v:vs) = showModel' 
                        (c <<< "(echo \""<<<v<<<": \") (eval "<<<v<<<")\n")
                        vs

getKey (I k) = k
getKey (B k) = k
getType (I _) = "int"
getType (B _) = "bool"

--printProgram :: Program x -> T.Text
showProgram p =    showVars (vars p) 
                <<< "(assert(and\n" 
                <<< foldr (\e c -> showExpr e <<< "\n" <<< c) T.empty (program p) 
                <<< "))\n"
                <<< showModel (if (null $ results p) then (map getKey (vars p)) else (results p))
printProgram  = putStr . T.unpack . showProgram

vs = ["b1", "b2", "b3"]
example :: Program Print
example = Program {
    vars    = map I vs,
    program = [
        -- all vars should be between 0 and 10
         and $ (map (\v -> vN v >=: 0) vs) ++ (map (\v -> vN v <=: 10) vs)
        -- one of them should be bigger than 5
        ,or $ (map (\v -> vN v >: 5) vs)
        ,vN "b1" =: 3
    ],
    results = []
}
