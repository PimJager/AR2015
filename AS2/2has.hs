{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (max, or, and, not)
import qualified Data.Text as T
import Control.Monad.Writer
import Data.Monoid
import Data.Char
import Data.List hiding (or, and)

type YicesWriter = Writer T.Text ()

timeunits = 20
bottles = [1,2,3]

max :: Int -> Int
max 1 = 144
max 2 = 80
max 3 = 16

start :: Int -> Int
start 1 = 3
start _ = 0

c i n = "j_" <<< showT i <<< "_" <<< showT n

variables :: YicesWriter
variables = do
    tellLn ";; define vars"
    mapM_ define bottles
    tellLn ""
    where
        define i = mapM_ (\n -> tellLn $ "(define " <<< c i n <<< "::int)") [1..timeunits]

bounds :: YicesWriter
bounds = do
    tellLn1 ";; all veriables shouwld be within bounds"
    mapM_ bounds' bottles
    tellLn ""
    where
        bounds' x =     mapM_ (\n -> tellLn1 $ geq (c x n) 0) [1..timeunits]
                    >>  mapM_ (\n -> tellLn1 $ leq (c x n) (max x)) [1..timeunits]

initial :: YicesWriter
initial = do
    tellLn1 ";; set initial values"
    tellLn1 $ equals (c 1 1) (start 1)
    tellLn1 $ equals (c 2 1) (start 2)
    tellLn1 $ equals (c 3 1) (start 3)
    tellLn ""

timestep :: Int -> YicesWriter
timestep n = do
    tellLn1 $ ";; timestep " <<< showT n
    tellLn1 ";; use 1 bottle to do action with"
    tellLn1 "(or " 
    mapM_ (timestep' n) bottles
    tellLn1 ")"
    tellLn ""
timestep' :: Int -> Int -> YicesWriter
timestep' n x = do
    tellLn2 $ ";; timestep " <<< showT n <<< "for bottle " <<< showT x
    tellLn2 $ or [fillOrEmpty n x, pour n x]
    where

        fillOrEmpty n x = and   [
                                 or [
                                     equals (c x n) (max x) --fill bottle to max
                                    ,equals (c x n) 0 -- empty bottle 
                                    ]
                                ,and $ map (\y -> equals' (c y n) (c y (n-1))) (delete x bottles) --leave all other bottles as last timestep
                                ]
        pour n x    = or $ map (\y -> pour' n x y) (delete x bottles)
        pour' n x y = and $ [
                                ite 
                                    ((c y (n-1)) `leq'` ((max x) -. (c x (n-1)))) 
                                    (and [equals (c y n) 0, equals' (c x n) (c x (n-1) +.. (c y (n-1)))])
                                    (and [equals (c x n) (max x), equals' (c y n) (c y (n-1) -.. (max x -. c x (n-1)))])
                            ] ++ map (\z -> equals' (c z n) (c z (n-1))) (delete x $ delete y bottles)
                              --leave bottles which are not poured as previous timestep                            

condition :: YicesWriter
condition = do
    tellLn1 ";; satisfying condition"
    tellLn1 $ or $ map (\n -> and [equals (c 1 n) 8, equals (c 2 n) 11]) [2..timeunits]

showModel :: YicesWriter
showModel = do
    tellLn ";; Show-model"
    mapM_ showC bottles
    where
        showC i = mapM_ (\n -> tellLn $ "(echo \""<<<c i n<<< ":  \")"<<<" (eval "<<<c i n<<<")") [1..timeunits]


main = putStr $ T.unpack $ execWriter env
    where
        env = do
            variables
            tellLn "(assert (and"
            bounds
            initial
            mapM_ timestep [2..timeunits]
            condition
            tellLn "))"
            tellLn "(check)"
            showModel





-- DSL?
equals :: (Show a) => T.Text -> a -> T.Text
equals a b = "(= " <<< a <<< " " <<< showT b <<<")"
equals' :: T.Text -> T.Text -> T.Text
equals' a b = "(= " <<< a <<< " " <<< b <<<")"
or :: [T.Text] -> T.Text
or as = foldr (<<<) ")" $ intersperse " " ("(or ":as)
and :: [T.Text] -> T.Text
and as = foldr (<<<) ")" $ intersperse " " ("(and ":as)
leq :: (Show a) => T.Text -> a -> T.Text
leq a b = "(<= " <<< a <<< " " <<< showT b <<<")"
leq' :: T.Text -> T.Text -> T.Text
leq' a b = "(<= " <<< a <<< " " <<< b <<<")"
geq :: (Show a) => T.Text -> a -> T.Text
geq a b = "(>= " <<< a <<< " " <<< showT b <<<")"
not :: T.Text -> T.Text
not a = "(not " <<< a <<< ")"

ite :: T.Text -> T.Text -> T.Text -> T.Text
ite c t e = "(ite " <<< c <<< " " <<< t <<< " " <<< e <<< ")" 
(+..) :: T.Text -> T.Text -> T.Text
(+..) a b = "(+ " <<< a <<< " " <<< b <<<")"
(-.) :: (Show a) => a -> T.Text -> T.Text
(-.) a b = "(- " <<< showT a <<< " " <<< b <<<")"
(-..) :: T.Text -> T.Text -> T.Text
(-..) a b = "(- " <<< a <<< " " <<< b <<<")"


tellLn :: T.Text -> Writer T.Text ()
tellLn w = tell (w `T.snoc` '\n')
tellLn1 w = tell "\t" >> tellLn w
tellLn2 w = tell "\t\t" >> tellLn w

showT :: (Show a) => a -> T.Text
showT = T.pack . show

(<<<) = T.append