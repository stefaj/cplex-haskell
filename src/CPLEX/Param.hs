{-# OPTIONS_GHC -Wall #-}

module CPLEX.Param ( CPX_PARAM(..)
                   , CPX_PROB_TYPE(..)
                   , CPX_CUT_TYPE(..)
                   , CPX_EFFORT_LEVEL(..)
                   , CPX_SOLUTION_CODE(..)
                   , intToSolution 
                   , effortLevelToInt 
                   , cutToInt
                   , paramToInt
                   , typeToInt
                   , intToType
                   ) where

import           Foreign.C(CInt)



data CPX_PARAM = CPX_PARAM_ADVIND
               | CPX_PARAM_AGGFILL
               | CPX_PARAM_AGGIND
               | CPX_PARAM_BASINTERVAL
               | CPX_PARAM_CFILEMUL
               | CPX_PARAM_CLOCKTYPE
               | CPX_PARAM_CRAIND
               | CPX_PARAM_DEPIND
               | CPX_PARAM_DPRIIND
               | CPX_PARAM_PRICELIM
               | CPX_PARAM_EPMRK
               | CPX_PARAM_EPOPT
               | CPX_PARAM_EPPER
               | CPX_PARAM_EPRHS
               | CPX_PARAM_FASTMIP
               | CPX_PARAM_SIMDISPLAY
               | CPX_PARAM_ITLIM
               | CPX_PARAM_ROWREADLIM
               | CPX_PARAM_NETFIND
               | CPX_PARAM_COLREADLIM
               | CPX_PARAM_NZREADLIM
               | CPX_PARAM_OBJLLIM
               | CPX_PARAM_OBJULIM
               | CPX_PARAM_PERIND
               | CPX_PARAM_PERLIM
               | CPX_PARAM_PPRIIND
               | CPX_PARAM_PREIND
               | CPX_PARAM_REINV
               | CPX_PARAM_REVERSEIND
               | CPX_PARAM_RFILEMUL
               | CPX_PARAM_SCAIND
               | CPX_PARAM_SCRIND
               | CPX_PARAM_SINGLIM
               | CPX_PARAM_SINGTOL
               | CPX_PARAM_TILIM
               | CPX_PARAM_XXXIND
               | CPX_PARAM_PREDUAL
               | CPX_PARAM_EPOPT_H
               | CPX_PARAM_EPRHS_H
               | CPX_PARAM_PREPASS
               | CPX_PARAM_DATACHECK
               | CPX_PARAM_REDUCE
               | CPX_PARAM_PRELINEAR
               | CPX_PARAM_LPMETHOD
               | CPX_PARAM_QPMETHOD
               | CPX_PARAM_WORKDIR
               | CPX_PARAM_WORKMEM
               | CPX_PARAM_THREADS
               | CPX_PARAM_CONFLICTDISPLAY
               | CPX_PARAM_SIFTDISPLAY
               | CPX_PARAM_SIFTALG
               | CPX_PARAM_SIFTITLIM
               | CPX_PARAM_MPSLONGNUM
               | CPX_PARAM_MEMORYEMPHASIS
               | CPX_PARAM_NUMERICALEMPHASIS
               | CPX_PARAM_FEASOPTMODE
               | CPX_PARAM_PARALLELMODE
               | CPX_PARAM_TUNINGMEASURE
               | CPX_PARAM_TUNINGREPEAT
               | CPX_PARAM_TUNINGTILIM
               | CPX_PARAM_TUNINGDISPLAY
               | CPX_PARAM_WRITELEVEL
               | CPX_PARAM_DETTILIM
               | CPX_PARAM_FILEENCODING
               | CPX_PARAM_APIENCODING
               | CPX_PARAM_SOLUTIONTARGET
               | CPX_PARAM_CLONELOG
               | CPX_PARAM_MIPCBREDLP
               | CPX_PARAM_MIPDISPLAY
               deriving Show

paramToInt :: Num a => CPX_PARAM -> a
paramToInt CPX_PARAM_ADVIND            = 1001
paramToInt CPX_PARAM_AGGFILL           = 1002
paramToInt CPX_PARAM_AGGIND            = 1003
paramToInt CPX_PARAM_BASINTERVAL       = 1004
paramToInt CPX_PARAM_CFILEMUL          = 1005
paramToInt CPX_PARAM_CLOCKTYPE         = 1006
paramToInt CPX_PARAM_CRAIND            = 1007
paramToInt CPX_PARAM_DEPIND            = 1008
paramToInt CPX_PARAM_DPRIIND           = 1009
paramToInt CPX_PARAM_PRICELIM          = 1010
paramToInt CPX_PARAM_EPMRK             = 1013
paramToInt CPX_PARAM_EPOPT             = 1014
paramToInt CPX_PARAM_EPPER             = 1015
paramToInt CPX_PARAM_EPRHS             = 1016
paramToInt CPX_PARAM_FASTMIP           = 1017
paramToInt CPX_PARAM_SIMDISPLAY        = 1019
paramToInt CPX_PARAM_ITLIM             = 1020
paramToInt CPX_PARAM_ROWREADLIM        = 1021
paramToInt CPX_PARAM_NETFIND           = 1022
paramToInt CPX_PARAM_COLREADLIM        = 1023
paramToInt CPX_PARAM_NZREADLIM         = 1024
paramToInt CPX_PARAM_OBJLLIM           = 1025
paramToInt CPX_PARAM_OBJULIM           = 1026
paramToInt CPX_PARAM_PERIND            = 1027
paramToInt CPX_PARAM_PERLIM            = 1028
paramToInt CPX_PARAM_PPRIIND           = 1029
paramToInt CPX_PARAM_PREIND            = 1030
paramToInt CPX_PARAM_REINV             = 1031
paramToInt CPX_PARAM_REVERSEIND        = 1032
paramToInt CPX_PARAM_RFILEMUL          = 1033
paramToInt CPX_PARAM_SCAIND            = 1034
paramToInt CPX_PARAM_SCRIND            = 1035
paramToInt CPX_PARAM_SINGLIM           = 1037
paramToInt CPX_PARAM_SINGTOL           = 1038
paramToInt CPX_PARAM_TILIM             = 1039
paramToInt CPX_PARAM_XXXIND            = 1041
paramToInt CPX_PARAM_PREDUAL           = 1044
paramToInt CPX_PARAM_EPOPT_H           = 1049
paramToInt CPX_PARAM_EPRHS_H           = 1050
paramToInt CPX_PARAM_PREPASS           = 1052
paramToInt CPX_PARAM_DATACHECK         = 1056
paramToInt CPX_PARAM_REDUCE            = 1057
paramToInt CPX_PARAM_PRELINEAR         = 1058
paramToInt CPX_PARAM_LPMETHOD          = 1062
paramToInt CPX_PARAM_QPMETHOD          = 1063
paramToInt CPX_PARAM_WORKDIR           = 1064
paramToInt CPX_PARAM_WORKMEM           = 1065
paramToInt CPX_PARAM_THREADS           = 1067
paramToInt CPX_PARAM_CONFLICTDISPLAY   = 1074
paramToInt CPX_PARAM_SIFTDISPLAY       = 1076
paramToInt CPX_PARAM_SIFTALG           = 1077
paramToInt CPX_PARAM_SIFTITLIM         = 1078
paramToInt CPX_PARAM_MPSLONGNUM        = 1081
paramToInt CPX_PARAM_MEMORYEMPHASIS    = 1082
paramToInt CPX_PARAM_NUMERICALEMPHASIS = 1083
paramToInt CPX_PARAM_FEASOPTMODE       = 1084
paramToInt CPX_PARAM_PARALLELMODE      = 1109
paramToInt CPX_PARAM_TUNINGMEASURE     = 1110
paramToInt CPX_PARAM_TUNINGREPEAT      = 1111
paramToInt CPX_PARAM_TUNINGTILIM       = 1112
paramToInt CPX_PARAM_TUNINGDISPLAY     = 1113
paramToInt CPX_PARAM_WRITELEVEL        = 1114
paramToInt CPX_PARAM_DETTILIM          = 1127
paramToInt CPX_PARAM_FILEENCODING      = 1129
paramToInt CPX_PARAM_APIENCODING       = 1130
paramToInt CPX_PARAM_SOLUTIONTARGET    = 1131
paramToInt CPX_PARAM_CLONELOG          = 1132
paramToInt CPX_PARAM_MIPCBREDLP        = 2055
paramToInt CPX_PARAM_MIPDISPLAY        = 2012

data CPX_PROB_TYPE = CPX_PROB_LP
                   | CPX_PROB_MILP
                   | CPX_PROB_FIXEDMILP
                   | CPX_PROB_QP
                   | CPX_PROB_MIQP
                   | CPX_PROB_FIXEDMIQP
                   | CPX_PROB_QCP
                   | CPX_PROB_MIQCP
                   deriving Show

typeToInt :: Num a => CPX_PROB_TYPE -> a
typeToInt CPX_PROB_LP        = 0
typeToInt CPX_PROB_MILP      = 1
typeToInt CPX_PROB_FIXEDMILP = 3
typeToInt CPX_PROB_QP        = 5
typeToInt CPX_PROB_MIQP      = 7
typeToInt CPX_PROB_FIXEDMIQP = 8
typeToInt CPX_PROB_QCP       = 10
typeToInt CPX_PROB_MIQCP     = 11

data CPX_CUT_TYPE = CPX_USECUT_FORCE
                | CPX_USECUT_PURGE
                | CPX_USECUT_FILTER

cutToInt :: CPX_CUT_TYPE -> CInt
cutToInt CPX_USECUT_FORCE = 0
cutToInt CPX_USECUT_PURGE = 1
cutToInt CPX_USECUT_FILTER = 2

data CPX_EFFORT_LEVEL = CPX_MIPSTART_AUTO
                      | CPX_MIPSTART_CHECKFEAS
                      | CPX_MIPSTART_SOLVEFIXED
                      | CPX_MIPSTART_SOLVEMIP
                      | CPX_MIPSTART_REPAIR

effortLevelToInt :: CPX_EFFORT_LEVEL -> CInt
effortLevelToInt CPX_MIPSTART_AUTO = 0
effortLevelToInt CPX_MIPSTART_CHECKFEAS = 1
effortLevelToInt CPX_MIPSTART_SOLVEFIXED = 2
effortLevelToInt CPX_MIPSTART_SOLVEMIP = 3
effortLevelToInt CPX_MIPSTART_REPAIR = 4

intToType :: (Eq a, Num a) => a -> Maybe CPX_PROB_TYPE
intToType  0 = Just CPX_PROB_LP
intToType  1 = Just CPX_PROB_MILP
intToType  3 = Just CPX_PROB_FIXEDMILP
intToType  5 = Just CPX_PROB_QP
intToType  7 = Just CPX_PROB_MIQP
intToType  8 = Just CPX_PROB_FIXEDMIQP
intToType 10 = Just CPX_PROB_QCP
intToType 11 = Just CPX_PROB_MIQCP
intToType  _ = Nothing

data CPX_SOLUTION_CODE = CPX_STAT_OPTIMAL
        | CPX_STAT_UNBOUNDED
        | CPX_STAT_INFEASIBLE
        | CPX_STAT_INForUNBD
        | CPX_STAT_OPTIMAL_INFEAS
        | CPX_STAT_NUM_BEST
        | CPX_STAT_ABORT_IT_LIM
        | CPX_STAT_ABORT_TIME_LIM
        | CPX_STAT_ABORT_OBJ_LIM
        | CPX_STAT_ABORT_USER
        | CPX_STAT_FEASIBLE_RELAXED_SUM
        | CPX_STAT_OPTIMAL_RELAXED_SUM
        | CPX_STAT_FEASIBLE_RELAXED_INF
        | CPX_STAT_OPTIMAL_RELAXED_INF
        | CPX_STAT_FEASIBLE_RELAXED_QUAD
        | CPX_STAT_OPTIMAL_RELAXED_QUAD
        | CPX_STAT_CONFLICT_FEASIBLE
        | CPX_STAT_CONFLICT_MINIMAL
        | CPX_STAT_CONFLICT_ABORT_CONTRADICTION
        | CPX_STAT_CONFLICT_ABORT_TIME_LIM
        | CPX_STAT_CONFLICT_ABORT_IT_LIM
        | CPX_STAT_CONFLICT_ABORT_NODE_LIM
        | CPX_STAT_CONFLICT_ABORT_OBJ_LIM
        | CPX_STAT_CONFLICT_ABORT_MEM_LIM
        | CPX_STAT_CONFLICT_ABORT_USER
        | CPX_STAT_OPTIMAL_FACE_UNBOUNDED
        | CPX_STAT_ABORT_PRIM_OBJ_LIM
        | CPX_STAT_ABORT_DUAL_OBJ_LIM
        | CPXMIP_OPTIMAL
        | CPXMIP_OPTIMAL_TOL
        | CPXMIP_INFEASIBLE
        | CPXMIP_SOL_LIM
        | CPXMIP_NODE_LIM_FEAS
        | CPXMIP_NODE_LIM_INFEAS
        | CPXMIP_TIME_LIM_FEAS
        | CPXMIP_TIME_LIM_INFEAS
        | CPXMIP_FAIL_FEAS
        | CPXMIP_FAIL_INFEAS
        | CPXMIP_MEM_LIM_FEAS
        | CPXMIP_MEM_LIM_INFEAS
        | CPXMIP_ABORT_FEAS
        | CPXMIP_ABORT_INFEAS
        | CPXMIP_OPTIMAL_INFEAS
        | CPXMIP_FAIL_FEAS_NO_TREE
        | CPXMIP_FAIL_INFEAS_NO_TREE
        | CPXMIP_UNBOUNDED
        | CPXMIP_INForUNBD
        | CPXMIP_FEASIBLE_RELAXED_SUM
        | CPXMIP_OPTIMAL_RELAXED_SUM
        | CPXMIP_FEASIBLE_RELAXED_INF
        | CPXMIP_OPTIMAL_RELAXED_INF
        | CPXMIP_FEASIBLE_RELAXED_QUAD
        | CPXMIP_OPTIMAL_RELAXED_QUAD
        | CPXMIP_ABORT_RELAXED
          deriving (Show, Eq)

intToSolution :: CInt -> CPX_SOLUTION_CODE
intToSolution   1  = CPX_STAT_OPTIMAL                         
intToSolution   2  = CPX_STAT_UNBOUNDED                       
intToSolution   3  = CPX_STAT_INFEASIBLE                      
intToSolution   4  = CPX_STAT_INForUNBD                       
intToSolution   5  = CPX_STAT_OPTIMAL_INFEAS                  
intToSolution   6  = CPX_STAT_NUM_BEST                        
intToSolution  10  = CPX_STAT_ABORT_IT_LIM                    
intToSolution  11  = CPX_STAT_ABORT_TIME_LIM                  
intToSolution  12  = CPX_STAT_ABORT_OBJ_LIM                   
intToSolution  13  = CPX_STAT_ABORT_USER                      
intToSolution  14  = CPX_STAT_FEASIBLE_RELAXED_SUM            
intToSolution  15  = CPX_STAT_OPTIMAL_RELAXED_SUM             
intToSolution  16  = CPX_STAT_FEASIBLE_RELAXED_INF            
intToSolution  17  = CPX_STAT_OPTIMAL_RELAXED_INF             
intToSolution  18  = CPX_STAT_FEASIBLE_RELAXED_QUAD           
intToSolution  19  = CPX_STAT_OPTIMAL_RELAXED_QUAD            
intToSolution  30  = CPX_STAT_CONFLICT_FEASIBLE               
intToSolution  31  = CPX_STAT_CONFLICT_MINIMAL                
intToSolution  32  = CPX_STAT_CONFLICT_ABORT_CONTRADICTION    
intToSolution  33  = CPX_STAT_CONFLICT_ABORT_TIME_LIM         
intToSolution  34  = CPX_STAT_CONFLICT_ABORT_IT_LIM           
intToSolution  35  = CPX_STAT_CONFLICT_ABORT_NODE_LIM         
intToSolution  36  = CPX_STAT_CONFLICT_ABORT_OBJ_LIM          
intToSolution  37  = CPX_STAT_CONFLICT_ABORT_MEM_LIM          
intToSolution  38  = CPX_STAT_CONFLICT_ABORT_USER             
intToSolution  20  = CPX_STAT_OPTIMAL_FACE_UNBOUNDED          
intToSolution  21  = CPX_STAT_ABORT_PRIM_OBJ_LIM              
intToSolution  22  = CPX_STAT_ABORT_DUAL_OBJ_LIM              
intToSolution 101  = CPXMIP_OPTIMAL                           
intToSolution 102  = CPXMIP_OPTIMAL_TOL                       
intToSolution 103  = CPXMIP_INFEASIBLE                        
intToSolution 104  = CPXMIP_SOL_LIM                           
intToSolution 105  = CPXMIP_NODE_LIM_FEAS                     
intToSolution 106  = CPXMIP_NODE_LIM_INFEAS                   
intToSolution 107  = CPXMIP_TIME_LIM_FEAS                     
intToSolution 108  = CPXMIP_TIME_LIM_INFEAS                   
intToSolution 109  = CPXMIP_FAIL_FEAS                         
intToSolution 110  = CPXMIP_FAIL_INFEAS                       
intToSolution 111  = CPXMIP_MEM_LIM_FEAS                      
intToSolution 112  = CPXMIP_MEM_LIM_INFEAS                    
intToSolution 113  = CPXMIP_ABORT_FEAS                        
intToSolution 114  = CPXMIP_ABORT_INFEAS                      
intToSolution 115  = CPXMIP_OPTIMAL_INFEAS                    
intToSolution 116  = CPXMIP_FAIL_FEAS_NO_TREE                 
intToSolution 117  = CPXMIP_FAIL_INFEAS_NO_TREE               
intToSolution 118  = CPXMIP_UNBOUNDED                         
intToSolution 119  = CPXMIP_INForUNBD                         
intToSolution 120  = CPXMIP_FEASIBLE_RELAXED_SUM              
intToSolution 121  = CPXMIP_OPTIMAL_RELAXED_SUM               
intToSolution 122  = CPXMIP_FEASIBLE_RELAXED_INF              
intToSolution 123  = CPXMIP_OPTIMAL_RELAXED_INF               
intToSolution 124  = CPXMIP_FEASIBLE_RELAXED_QUAD             
intToSolution 125  = CPXMIP_OPTIMAL_RELAXED_QUAD              
intToSolution 126  = CPXMIP_ABORT_RELAXED                    
