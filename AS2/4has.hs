{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Yices as Y
import qualified Data.Text as T

maxX = 4
maxY = 4
maxSteps = 3

allCells    = [(x,y) | x<-[1..maxX], y<-[1..maxY]]
c (x,y,s)   = T.concat ["c_", T.pack $ show $ x, "_", T.pack $ show $ y, "_", T.pack $ show $ s]
n (x,y,s)   = T.concat ["n_", T.pack $ show $ x, "_", T.pack $ show $ y, "_", T.pack $ show $ s]
vars steps  = map c [(x,y,s) | (x,y)<-allCells, s<-steps] 
neighVars   = map n [(x,y,s) | (x,y)<-allCells, s<-[2..maxSteps]] 
            
neighbours (x,y) = filter   
                        (\(x',y')->x'>=1 && x'<=maxX && y'>=1 && y'<=maxY && not (x'==x && y'==y)) 
                        [(x',y') | x'<-[x-1,x,x+1], y'<-[y-1,y,y+1]]

-- cells are either living or dead
range  = Y.and $ map (\v -> Y.or [Y.vN v Y.=: 0, Y.vN v Y.=: 1]) (vars [1..maxSteps])

-- all cells should be dead at the last iteration
allDead = Y.and $ map (\v -> Y.vN v Y.=: 0) (vars [maxSteps])

--help variables for the number of living neighbours
livingNeighbours :: Int -> Y.Print Bool
livingNeighbours s = Y.and $ map 
                            (\(x,y) -> Y.vN (n (x,y,s)) Y.=: 
                                    sum (map (\(x',y') -> Y.vN (c (x',y',s-1))) (neighbours (x,y)))
                            ) 
                            allCells

ruleLonely :: Int -> Y.Print Bool
-- cells die if they have less then two living neighbours
ruleLonely s = Y.and $ map (\(x,y) -> Y.ite 
                                        (Y.vN (n (x,y,s)) Y.<: 2)
                                        (Y.vN (c (x,y,s)) Y.=: 0)
                                        (1 Y.=: 1)
                            )
                            allCells

ruleSurvive :: Int -> Y.Print Bool
ruleSurvive s = Y.and $ map (\(x,y) -> Y.ite 
                                        (Y.vN (n (x,y,s)) Y.=: 2)
                                        (Y.vN (c (x,y,s)) Y.=: Y.vN (c (x,y,s-1)) )
                                        (1 Y.=: 1)
                            )
                            allCells

ruleBorn :: Int -> Y.Print Bool
ruleBorn s = Y.and $ map (\(x,y) -> Y.ite 
                                        (Y.vN (n (x,y,s)) Y.=: 3)
                                        (Y.vN (c (x,y,s)) Y.=: 1)
                                        (1 Y.=: 1)
                            )
                            allCells

ruleStarve :: Int -> Y.Print Bool
ruleStarve s = Y.and $ map (\(x,y) -> Y.ite 
                                        (Y.vN (n (x,y,s)) Y.>=: 4)
                                        (Y.vN (c (x,y,s)) Y.=: 0)
                                        (1 Y.=: 1)
                            )
                            allCells

prog = Y.Program {
    Y.vars = map Y.I (vars [1..maxSteps]) ++ map Y.I neighVars,
    Y.program = [
         range
        ,allDead
        ,Y.and $ map livingNeighbours [2..maxSteps]
        ,Y.and $ map ruleLonely [2..maxSteps]
        ,Y.and $ map ruleSurvive [2..maxSteps]
        ,Y.and $ map ruleBorn [2..maxSteps]
        ,Y.and $ map ruleStarve [2..maxSteps]
    ],
    --the start configuration is the intersting one
    Y.results = vars [maxSteps]
}

main = Y.printProgram prog