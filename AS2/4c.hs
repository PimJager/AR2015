{-# LANGUAGE OverloadedStrings #-}

-- ghc 4a.hs; rm 4a.yic; ./4a >> 4a.yic; time yices 4a.yic

module Main where

import qualified Data.Text.Yices as Y
import qualified Data.Text as T

maxX = 5
maxY = 5
maxSteps = 80
goal = 16

alive = 1
dead = 0

allCells    = [(x,y) | x<-[1..maxX], y<-[1..maxY]]
c (x,y,s)   = T.concat ["c_", T.pack $ show $ x, "_", T.pack $ show $ y, "_", T.pack $ show $ s]
n (x,y,s)   = T.concat ["n_", T.pack $ show $ x, "_", T.pack $ show $ y, "_", T.pack $ show $ s]
vars steps  = map c [(x,y,s) | (x,y)<-allCells, s<-steps] 
neighVars   = map n [(x,y,s) | (x,y)<-allCells, s<-[2..maxSteps]] 
            
neighbours (x,y) = filter   
                        (\(x',y')->x'>=1 && x'<=maxX && y'>=1 && y'<=maxY && not (x'==x && y'==y)) 
                        [(x',y') | x'<-[x-1,x,x+1], y'<-[y-1,y,y+1]]

-- cells are either living or dead
range :: Y.Print Bool
range  = Y.and $ map (\v -> Y.or [Y.vN v Y.=: dead, Y.vN v Y.=: alive]) (vars [1..maxSteps])


livingNeighbours :: (Int,Int,Int) -> Y.Print Int
livingNeighbours (x,y,s) = sum $ map (\(x',y') -> Y.vN (c (x',y',s))) (neighbours (x,y))


ruleLonely :: Int -> Y.Print Bool
-- cells die if they have less then two living neighbours
ruleLonely s = Y.and $ map (\(x,y) -> Y.ite 
                                        (livingNeighbours (x,y,s) Y.<: 2)
                                        (Y.vN (c (x,y,s)) Y.=: dead)
                                        (1 Y.=: 1)
                            )
                            allCells

ruleSurvive :: Int -> Y.Print Bool
ruleSurvive s = Y.and $ map (\(x,y) -> Y.ite 
                                        (livingNeighbours (x,y,s) Y.=: 2)
                                        (Y.vN (c (x,y,s)) Y.=: Y.vN (c (x,y,s-1)) )
                                        (1 Y.=: 1)
                            )
                            allCells

ruleBorn :: Int -> Y.Print Bool
ruleBorn s = Y.and $ map (\(x,y) -> Y.ite 
                                        (livingNeighbours (x,y,s) Y.=: 3)
                                        (Y.vN (c (x,y,s)) Y.=: alive)
                                        (1 Y.=: 1)
                            )
                            allCells

ruleStarve :: Int -> Y.Print Bool
ruleStarve s = Y.and $ map (\(x,y) -> Y.ite 
                                        (livingNeighbours (x,y,s) Y.>=: 4)
                                        (Y.vN (c (x,y,s)) Y.=: dead)
                                        (1 Y.=: 1)
                            )
                            allCells

sat :: Y.Print Bool
sat = Y.and $ map (\v -> Y.vN v Y.=: 1) $ map (\(x,y) -> c (x,y,maxSteps)) [(3,3),(3,4),(4,3),(4,4)]

-- all cells should be dead at the last iteration
allDead :: Y.Print Bool
allDead = Y.and $ map (\v -> Y.vN v Y.=: dead) (vars [maxSteps])

allAlive :: Y.Print Bool
allAlive = Y.and $ map (\v -> Y.vN v Y.=: alive) (vars [maxSteps])

halfAlive :: Y.Print Bool
halfAlive = sum (map Y.vN $ vars [maxSteps]) Y.>=: goal

prog = Y.Program {
    Y.vars = map Y.I (vars [1..maxSteps]),
    Y.program = [
         range
        ,Y.and $ map ruleLonely [2..maxSteps]
        ,Y.and $ map ruleSurvive [2..maxSteps]
        ,Y.and $ map ruleBorn [2..maxSteps]
        ,Y.and $ map ruleStarve [2..maxSteps]
        ,halfAlive
    ],
    --the start configuration is the intersting one
    Y.results = vars [maxSteps]
}

main = Y.printProgram prog