{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Vector as V

import Foreign.C

import CPLEX.Param
import CPLEX.Core

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

main :: IO ()
main = sol' >>= print

sol' :: IO ()
sol' = withEnv $ \env -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp -> do
    let objsen = CPX_MAX
        obj = V.fromList [1,2,3]
        rhs = V.fromList [L 20, L 30]
        xbnds = [(Just 0, Just 40), (Just 0, Nothing), (Just 0, Nothing)]
        amat = [ (Row 0, Col 0, -1)
               , (Row 1, Col 0, 1)
               , (Row 0, Col 1, 1)
               , (Row 1, Col 1, -3)
               , (Row 0, Col 2, 1)
               , (Row 1, Col 2, 1)
               ]
    statusLp <- copyLp env lp objsen obj rhs amat (V.fromList xbnds)

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    ------------------------
    let qmat = [ (Row 0, Col 0, -33)
               , (Row 1, Col 0, 6)
               , (Row 0, Col 1, 6)
               , (Row 1, Col 1, -22)
               , (Row 2, Col 1, 11.5)
               , (Row 1, Col 2, 11.5)
               , (Row 2, Col 2, -11)
               ]
    statusQuad <- copyQuad env lp qmat
    case statusQuad of
      Nothing -> return ()
      Just msg -> error $ "CPXcopyquad error: " ++ msg

    ------------------------
    statusOpt <- qpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg
      
    statusSol <- getSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do
        putStrLn $ "x      : " ++ show (solX sol)
        putStrLn $ "pi'    : " ++ show (solPi sol)
        putStrLn $ "slack  : " ++ show (solSlack sol)
        putStrLn $ "dj     : " ++ show (solDj sol)
        putStrLn $ "solstat: " ++ show (solStat sol)
        putStrLn $ "objval : " ++ show (solObj sol)
