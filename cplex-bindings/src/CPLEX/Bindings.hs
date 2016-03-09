{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CPLEX.Bindings ( CpxEnv'
                      , CpxLp'
                      , c_CPXopenCPLEX
                      , c_CPXcloseCPLEX
                      , c_CPXcreateprob
                      , c_CPXfreeprob
                      , c_CPXnewrows
                      , c_CPXaddrows
                      , c_CPXnewcols
                      , c_CPXaddcols
                      , c_CPXchgcoeflist
                      , c_CPXchgrhs
                      , c_CPXchgrngval
                      , c_CPXchgbds
                      , c_CPXchgobj
                      , c_CPXchgsense
                      , c_CPXchgqpcoef
                      , c_CPXchgprobtype
                      , c_CPXcopyctype
                      , c_CPXgeterrorstring
                      , c_CPXgetstatstring
                      , c_CPXsetintparam
                      , c_CPXsetdblparam
                      , c_CPXgetnumcols
                      , c_CPXgetnumrows
                      , c_CPXgetobjval
                      , c_CPXgetx
                      , c_CPXgetstat
                      , c_CPXgetslack
                      , c_CPXcopylp
--                      , c_CPXcheckcopylp
                      , c_CPXcopyquad
--                      , c_CPXcheckcopyquad
                      , c_CPXqpopt
                      , c_CPXlpopt
                      , c_CPXmipopt
                      , c_CPXprimopt
                      , c_CPXdualopt
                      , c_CPXhybnetopt
                      , c_CPXsiftopt
                      , c_CPXsolution
                      , c_CPXwriteprob
                      , c_CPXaddusercuts
                      , CIncumbentCallback
                      , c_createIncumbentCallbackPtr
                      , c_CPXsetincumbentcallbackfunc
                      ) where

import           Foreign.C   (CChar (..), CDouble (..), CInt (..))
import           Foreign.Ptr (Ptr, FunPtr)

data CpxEnv'
data CpxLp'

foreign import ccall unsafe "cplex.h CPXopenCPLEX" c_CPXopenCPLEX :: Ptr CInt -> IO (Ptr CpxEnv')
foreign import ccall unsafe "cplex.h CPXcloseCPLEX" c_CPXcloseCPLEX :: Ptr (Ptr CpxEnv') -> IO CInt

foreign import ccall unsafe "cplex.h CPXcreateprob" c_CPXcreateprob ::
  Ptr CpxEnv' -> Ptr CInt -> Ptr CChar -> IO (Ptr CpxLp')
foreign import ccall unsafe "cplex.h CPXfreeprob" c_CPXfreeprob ::
  Ptr CpxEnv' -> Ptr (Ptr CpxLp') -> IO CInt

foreign import ccall unsafe "cplex.h CPXnewrows" c_CPXnewrows ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXaddrows" c_CPXaddrows ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXnewcols" c_CPXnewcols ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXaddcols" c_CPXaddcols ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgcoeflist" c_CPXchgcoeflist ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgrhs" c_CPXchgrhs ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgobj" c_CPXchgobj ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgbds" c_CPXchgbds ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CInt -> Ptr CChar -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgrngval" c_CPXchgrngval ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgsense" c_CPXchgsense ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> Ptr CInt -> Ptr CChar -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgqpcoef" c_CPXchgqpcoef ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> CInt -> CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgprobtype" c_CPXchgprobtype ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> IO CInt

foreign import ccall unsafe "cplex.h CPXcopyctype" c_CPXcopyctype ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CChar -> IO CInt

foreign import ccall unsafe "cplex.h CPXgeterrorstring" c_CPXgeterrorstring ::
  Ptr CpxEnv' -> CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "cplex.h CPXgetstatstring" c_CPXgetstatstring ::
  Ptr CpxEnv' -> CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "cplex.h CPXsetintparam" c_CPXsetintparam ::
  Ptr CpxEnv' -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "cplex.h CPXsetdblparam" c_CPXsetdblparam ::
  Ptr CpxEnv' -> CInt -> CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetnumcols" c_CPXgetnumcols ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetnumrows" c_CPXgetnumrows ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetobjval" c_CPXgetobjval ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetx" c_CPXgetx ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CDouble -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetstat" c_CPXgetstat ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetslack" c_CPXgetslack ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CDouble -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "cplex.h CPXcopylp" c_CPXcopylp ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> -- (CPXCENVptr env, CpxLp'ptr lp, int numcols,
  CInt -> CInt -> Ptr CDouble ->     --  int numrows, int objsense, const double *objective,
  Ptr CDouble -> Ptr CChar ->        --  const double *rhs, const char *sense,
  Ptr CInt -> Ptr CInt ->            --  const int *matbeg, const int *matcnt,
  Ptr CInt -> Ptr CDouble ->         --  const int *matind, const double *matval,
  Ptr CDouble -> Ptr CDouble ->      --  const double *lb, const double *ub,
  Ptr CDouble                        --  const double *rngval);
  -> IO CInt

--foreign import ccall unsafe "cplexcheck.h CPXcheckcopylp" c_CPXcheckcopylp ::
--                                         "CPXcheckcopylp"
--  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> -- (CPXCENVptr env, CpxLp'ptr lp, int numcols,
--  CInt -> CInt -> Ptr CDouble ->     --  int numrows, int objsense, const double *objective,
--  Ptr CDouble -> Ptr CChar ->        --  const double *rhs, const char *sense,
--  Ptr CInt -> Ptr CInt ->            --  const int *matbeg, const int *matcnt,
--  Ptr CInt -> Ptr CDouble ->         --  const int *matind, const double *matval,
--  Ptr CDouble -> Ptr CDouble ->      --  const double *lb, const double *ub,
--  Ptr CDouble                        --  const double *rngval);
--  -> IO CInt


foreign import ccall unsafe "cplex.h CPXcopyquad" c_CPXcopyquad ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CInt -> -- (CPXCENVptr env, CpxLp'ptr lp, const int *qmatbeg,
  Ptr CInt -> Ptr CInt ->                --  const int *qmatcnt, const int *qmatind,
  Ptr CDouble ->                         --  const double *qmatval);
  IO CInt

--foreign import ccall unsafe "cplexcheck.h CPXcheckcopyquad" c_CPXcheckcopyquad ::
--  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CInt -> -- (CPXCENVptr env, CpxLp'ptr lp, const int *qmatbeg,
--  Ptr CInt -> Ptr CInt ->                --  const int *qmatcnt, const int *qmatind,
--  Ptr CDouble ->                         --  const double *qmatval);
--  IO CInt

foreign import ccall unsafe "cplex.h CPXqpopt" c_CPXqpopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXlpopt" c_CPXlpopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXmipopt" c_CPXmipopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXprimopt" c_CPXprimopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXdualopt" c_CPXdualopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXhybnetopt" c_CPXhybnetopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> IO CInt

foreign import ccall unsafe "cplex.h CPXsiftopt" c_CPXsiftopt ::
  Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt

foreign import ccall unsafe "cplex.h CPXsolution" c_CPXsolution ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CInt ->       -- (CPXCENVptr env, CPXCLPptr lp, int *lpstat_p,
  Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> --  double *objval_p, double *x, double *pi,
  Ptr CDouble -> Ptr CDouble -> IO CInt        --  double *slack, double *dj);

foreign import ccall unsafe "cplex.h CPXwriteprob" c_CPXwriteprob ::
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CChar -> Ptr CChar -> IO CInt


--new
-- http://www.ibm.com/support/knowledgecenter/SSSA5P_12.2.0/ilog.odms.cplex.help/html/refcallablelibrary/html/functions/CPXaddusercuts.html?lang=en
foreign import ccall unsafe "cplex.h CPXaddusercuts" c_CPXaddusercuts ::
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt
  -- int CPXaddusercuts(CPXCENVptr env, CPXLPptr lp, int rcnt, int nzcnt, const double * rhs, const char * sense, const int * rmatbeg, const int * rmatind, const double * rmatval, char ** rowname)
  -- int CPXaddrows (CPXENVptr env,
  --               CPXLPptr lp,
  --               int ccnt,
  --               int rcnt,
  --               int nzcnt,
  --               double *rhs,
  --               char *sense,
  --               int *rmatbeg,
  --               int *rmatind,
  --               double *rmatval,
  --               char **colname,
  --               char **rowname);


foreign import ccall unsafe "cplex.h CPXsetincumbentcallbackfunc" c_CPXsetincumbentcallbackfunc::
    Ptr CpxEnv' -> FunPtr CIncumbentCallback -> Ptr () -> CInt


    -- int callback (CPXCENVptr env,
    --               void *cbdata,
    --               int wherefrom,
    --               void *cbhandle,
    --               double objval,
    --               double *x,
    --               int *isfeas_p,
    --               int *useraction_p);

type CIncumbentCallback = Ptr CpxEnv' -> Ptr () -> CInt -> Ptr () -> CDouble -> Ptr CInt -> Ptr Int -> Ptr Int -> IO Int

--foreign import ccal unsafe "wrapper" c_createIncumbentCallbackPtr :: (CIncumbentCallback) -> IO (FunPtr (CIncumbentCallback))

foreign import ccall "wrapper"
  c_createIncumbentCallbackPtr :: CIncumbentCallback -> IO (FunPtr (CIncumbentCallback))
--int CPXsetincumbentcallbackfunc(CPXENVptr env, int(*)(CALLBACK_INCUMBENT_ARGS) incumbentcallback, void * cbhandle)
