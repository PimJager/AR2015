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

data Component = C { ci:: Int, cw :: Int, ch :: Int } 
    deriving (Eq, Show)

powerC      = map (\i-> C i 2 4) [1..3]
regularC    = zipWith (\i (w,h) -> C i w h) [4..] [(9,7), (12,6), (10,7), (18,5), (20,4), (10,6), (8,6), (10,8)] 
dimW        = 29
dimH        = 22

x c = "x_" <<< (showT $ ci c)
y c = "y_" <<< (showT $ ci c)
w c = "w_" <<< (showT $ ci c)
h c = "h_" <<< (showT $ ci c)

variables :: [Component] -> YicesWriter
variables cs = do
    tellLn ";; Define all vars"
    mapM_ components cs
    tellLn ""
    where
        components c = mapM_ (\f-> defV f c) [x,y,w,h]
        defV f c = tellLn $ "(define " <<< (f c) <<< "::int)"

dimensions :: [Component] -> YicesWriter
dimensions cs = do
    tellLn "\t;; Components dimensions"      
    mapM_ components cs
    tellLn ""
    where
        components c = do 
            tellLn  $ "(or (and"
            tellLn  $ "\t (= " <<< w c <<< " " <<< (showT . cw) c <<< ")"
            tellLn  $ "\t (= " <<< h c <<< " " <<< (showT . ch) c <<< ")"
            tellLn  $ "))"
            --tellLn  $ ") (and "
            --tellLn  $ "\t (= " <<< w c <<< " " <<< (showT . ch) c <<< ")"
            --tellLn  $ "\t (= " <<< h c <<< " " <<< (showT . cw) c <<< ")"
            --tellLn  $ "))"


inside :: [Component] -> YicesWriter
inside cs = do 
    tellLn "\t;; Components should fit in the chip"
    mapM_ components cs
    tellLn ""
    where
        components c = do
            tell    $ "\t (<= 0 " <<< x c <<< ") "
            tellLn  $ "(< " <<< x c <<< " (- " <<< showT dimW <<< " " <<< w c <<< "))"
            tell    $ "\t (<= 0 " <<< y c <<< ") "
            tellLn  $ "(< " <<< y c <<< " (- " <<< showT dimH <<< " " <<< h c <<< "))"

overlap :: [Component] -> YicesWriter
overlap cs = do
    tellLn "\t;; components should not overlap"
    mapM_ components
     [(c1,c2) | c1 <- cs, c2<-(delete c1 cs)]
    tellLn ""
    where
        components (c1,c2) = do
            tellLn  $ "\t (or "
            tellLn  $ "\t\t (>= " <<< left c1 <<< " " <<< right c2 <<< ")"
            tellLn  $ "\t\t (<= " <<< right c1 <<< " " <<< left c2 <<< ")"
            tellLn  $ "\t\t (>= " <<< bottom c1 <<< " " <<< top c2 <<< ")"
            tellLn  $ "\t\t (<= " <<< top c1 <<< " " <<< bottom c2 <<< ")"
            tellLn  $ "\t )"
        bottom  = y
        top c   = "(+ " <<< y c <<< " " <<< h c <<< ")"
        left    = x
        right c = "(+ " <<< x c <<< " " <<< w c <<< ")"

heat :: [Component] -> YicesWriter
heat cs = do
    tellLn "\t;; Due to heat power components can not be to close"
    mapM_ components [(c1,c2) | c1 <- cs, c2<-(delete c1 cs)]
    tellLn ""
    where
        components (c1,c2) = do
            tellLn  $ "\t (or (>= (abs (- (+ " <<< x c1 <<< " " <<< (half w c1) <<< ")" <<< "(+ " <<< x c2 <<< " " <<< (half w c2) <<< ")" <<< ")) 17)"
            tellLn  $ "\t\t (>= (abs (- (+ " <<< y c1 <<< " " <<< (half h c1) <<< ")" <<< "(+ " <<< y c2 <<< " " <<< (half h c2) <<< ")" <<< ")) 17))"
        half f c = "(/ " <<< f c <<< " 2)"

power :: [Component] -> [Component] -> YicesWriter
power pcs rcs = do
    tellLn "\t;; Alle components should have atleast 1 edge contact with a power component"
    mapM_ regular rcs
    tellLn ""
    where
        regular c = do
            tellLn "\t (or "
            mapM_ (power' c) pcs
            tellLn "\t )"
        power' rc pc = do
            tellLn  $ "\t\t(or\t(and "
            tellLn  $ "\t\t\t (<=  " <<< x pc <<< " " <<< x rc <<< ") (< " <<< x rc <<< " (+ " <<< x pc <<< " " <<< w pc <<< "))"
            tellLn  $ "\t\t\t (or (= " <<< y rc <<< "(+ " <<< y pc <<< " " <<< h pc <<< ")) (= " <<< y pc <<< "(+ " <<< y rc <<< " " <<< h rc <<< ")))"
            tellLn  $ "\t\t\t)"
            tellLn  $ "\t\t\t(and "
            tellLn  $ "\t\t\t (<=  " <<< y pc <<< " " <<< y rc <<< ") (< " <<< y rc <<< " (+ " <<< y pc <<< " " <<< h pc <<< "))"
            tellLn  $ "\t\t\t (or (= " <<< x rc <<< "(+ " <<< x pc <<< " " <<< w pc <<< ")) (= " <<< x pc <<< "(+ " <<< x rc <<< " " <<< w rc <<< ")))"
            tellLn "\t\t))"

results :: [Component] -> YicesWriter
results cs = do
    tellLn ";; SHow output"
    tellLn "(check)"
    tellLn "(echo \"x/y values:\")"
    mapM_ components cs
    tellLn ""
    where
        components c = do
            tellLn  $ "(echo \"" <<< x c <<< ":\" ) (eval " <<< x c <<< ")"
            tellLn  $ "(echo \"" <<< y c <<< ":\" ) (eval " <<< y c <<< ")"
            tellLn  $ "(echo \"" <<< w c <<< ":\" ) (eval " <<< w c <<< ")"
            tellLn  $ "(echo \"" <<< h c <<< ":\" ) (eval " <<< h c <<< ")"

main = putStr $ T.unpack $ execWriter $ (variables (powerC++regularC) >> env >> results (powerC++regularC))
    where
        env = do
            tellLn "(assert (or"
            mapM_ writers permutations
            tellLn "))"
        writers (pc, rc) = do -- all writers expect to be executed within the aseert - or block
            tellLn "  (and"
            dimensions (pc++rc)
            inside (pc++rc)
            overlap (pc++rc)
            heat pc
            power pc rc
            tellLn "  )"
        permutations                = [(powerC, regularC)]
        permutations_                = [(pcs, rcs) |  pcs <- (permutations'' $ permutations' powerC), rcs <- (permutations'' $ permutations' regularC)]
        permutations'               = map (\(c) -> (C (ci c) (cw c) (ch c), C (ci c) (ch c) (cw c)))
        permutations'' []           = [[]]
        permutations'' ((c,c'):cs)  = (map (c:) (permutations'' cs)) ++ (map (c':) (permutations'' cs))