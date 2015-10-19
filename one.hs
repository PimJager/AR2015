{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text as T
import Control.Monad.Writer as W
import Data.Char

num_prittles    = 20
num_trucks      = 6
capacity_kg     = 7800
capacity_p      = 8

data Pallet = Pallet {p_count :: Int, p_weight :: Int, p_name :: T.Text}
nuzzle      = Pallet {p_count =  4, p_weight =  700, p_name = "nuzzle"}
stipple     = Pallet {p_count =  8, p_weight = 1000, p_name = "stipple"}
crottle     = Pallet {p_count = 10, p_weight = 1500, p_name = "crottle"}
dupple      = Pallet {p_count =  5, p_weight =  100, p_name = "dupple"}
prittle     = Pallet {p_count = num_prittles, p_weight = 800, p_name = "prittle"}
allPallets  = [nuzzle, stipple, crottle, dupple, prittle]

showT :: Show a => a -> Text
showT a = T.pack $ show a

type YicesWriter = W.Writer T.Text ()

tellLn :: T.Text -> W.Writer T.Text ()
tellLn w = tell (w `snoc` '\n')

pallet :: Pallet -> Int -> Int -> Text
pallet p t n = (p_name p) `snoc`(intToDigit t) `snoc` '_' `mappend` (showT n)

defineVars :: YicesWriter
defineVars = do
    tellLn ";; Define all the transportable packets"
    mapM_ pallets allPallets
    tellLn ""
    where
        deff n      = "(define " `T.append` n `T.append` "::int)"
        pallets p   = mapM_ (\n -> mapM_ (\t -> tellLn $ deff $ pallet p t n) [1..num_trucks]) [1..p_count p] 

varRange :: YicesWriter
varRange = do
    tellLn ";; All vars should be between 1 and 0"
    mapM_ range allPallets
    tellLn ""
    where
        range p     = mapM_ (\n -> mapM_ (\t -> range' p t n) [1..num_trucks]) [1..p_count p]
        range' p t n= tell "(assert( and(<= 0 " >> tell (pallet p t n) >> tell ") (<= " >> tell (pallet p t n) >> tellLn " 1)))"

unique :: YicesWriter
unique = do
    tellLn ";; All pallets can be in only _one_ truck"
    mapM_ pallets allPallets
    tellLn ""
    where
        pallets :: Pallet -> YicesWriter
        pallets p  =    tell "(assert (and "
                    >>  mapM_ (trucks p) [1..p_count p]
                    >>  tellLn "))"
        trucks :: Pallet -> Int -> YicesWriter
        trucks p n =    tell "(= (+"
                    >>  mapM_ (\t-> tell " " >> tell (pallet p t n)) [1..num_trucks]
                    >>  tellLn ") 1)"

amount :: YicesWriter
amount = do
    tellLn ";; Number of packets to be transported"
    mapM_ pallets allPallets
    tellLn ""
    where
        pallets :: Pallet -> YicesWriter
        pallets p =     tell "(assert (= (+" 
                    >>  mapM_ (\n -> mapM_ (\t -> tell " " >> tell (pallet p t n)) [1..num_trucks]) [1..p_count p]
                    >>  tell ") " >> tell (showT $ p_count p) >> tellLn " ))"

capacityP :: YicesWriter
capacityP = do
    tellLn ";; Trucks can carry only 8 pallets"
    mapM_ trucks[1..num_trucks]
    tellLn ""
    where
        trucks t =      tell "(assert (<= (+" 
                    >>  mapM_ (\p -> mapM_ (\n -> tell " " >> tell (pallet p t n)) [1..p_count p]) allPallets
                    >>  tell ") " >> tell (showT capacity_p) >> tellLn " ))"

capacityKG :: YicesWriter
capacityKG = do
    tellLn ";; Trucks should not carry to much weight"
    mapM_ trucks [1..num_trucks]
    tellLn ""
    where
        trucks t =      tell "(assert (<= (+"
                    >>  mapM_ (\p -> 
                            mapM_ (\n -> 
                                tell " (* " >> tell (pallet p t n) >> tell " " >> tell (showT $ p_weight p) >> tell ")") 
                                [1..p_count p]) 
                            allPallets
                    >>  tell ") " >> tell (showT capacity_kg) >> tellLn " ))"

cooled :: YicesWriter
cooled = do
    tellLn ";; Stipples should be cooled in the first two trucks"
    tell "(assert (= (+"
    mapM_ (\n -> tell " " >> tell (pallet stipple 1 n) >> tell " " >> tell (pallet stipple 2 n)) [1..p_count stipple]
    tell ") " >> tell (showT $ p_count stipple) >> tellLn " ))"
    tellLn ""

expensive :: YicesWriter
expensive = do
    tellLn ";; Duples are expensive, max 2 per truck"
    mapM_ trucks [1..num_trucks]
    tellLn ""
    where 
        trucks t =      tellLn "(assert (<= (+"
                    >>  mapM_ (\n -> tell " " >> tell (pallet dupple t n)) [1..p_count dupple]
                    >>  tellLn ") 2 ))"

explosive :: YicesWriter
explosive = do
    tellLn ";; Prittles and crottles can't be in the same truck"
    mapM_ (trucks_ crottle prittle) [1..num_trucks]
    mapM_ (trucks_ prittle crottle) [1..num_trucks]
    tellLn ""
    where
        trucks_ :: Pallet -> Pallet -> Int -> YicesWriter
        trucks_ p1 p2 t = -- if |#p1 in truck|>0 then |#p2 in truck|=0 else true
                tell "(assert (ite (> (+ " 
            >>  mapM_ (\n -> tell " " >> tell (pallet p1 t n)) [1..p_count p1]
            >>  tell ") 0) (= (+"
            >>  mapM_ (\n -> tell " " >> tell (pallet p2 t n)) [1..p_count p2]
            >>  tellLn ") 0) true))"

showModel :: YicesWriter
showModel = tellLn "(check)" >> tellLn "(show-model)" 


main = putStr $ T.unpack $ execWriter   (   defineVars 
                                        >>  varRange 
                                        >>  amount
                                        >>  unique
                                        >>  capacityP 
                                        >>  capacityKG
                                        >>  cooled 
                                        >>  expensive 
                                        >>  explosive
                                        >>  showModel
                                        )