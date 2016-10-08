{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPLEX.Core ( CpxEnv(..)
             , CpxLp
             , ObjSense(..)
             , Sense(..)
             , Bound(..)
             , Type(..)
             , Row(..)
             , Col(..)
             , CpxSolution(..)
               -- * high level bindings
             , openCPLEX
             , closeCPLEX
             , createProb
             , freeProb
             , setIntParam
             , setDoubleParam
             , copyLp
               --, checkCopyLp
             , copyQuad
               --, checkCopyQuad
             , copyMip
             , qpopt
             , lpopt
             , mipopt
             , primopt
             , dualopt
             , siftopt
             , hybnetopt
             , writeprob
               -- * change things
             , changeCoefList
             , changeObj
             , changeRhs
             , changeBds
             , changeRngVal
             , changeSens
             , changeQpCoef
               -- * low level stuff
             , getNumCols
             , getNumRows
             , getErrorString
             , getStatString
             , getBaseVars
             -- MIP
             , setIncumbentCallback
             , setCutCallback
             , setLazyConstraintCallback  
             , addCuts
             , addRows
             , addLazyConstraints
             , getCallbackLP
             , getCallbackNodeX
             , addCutFromCallback
             , addSingleMIPStart
             , getSolution
             , getMIPSolution
             , getMipRelGap
             , getMipBestInteger 
               -- * convenience wrappers
             , withEnv
             , withLp
             ) where

-- import           Control.Monad.Error
import           Control.Monad.IO.Class
import qualified Control.Monad.Primitive as Prim
-- import           Control.Monad.Trans
-- import           Control.Monad.Trans.Maybe
import qualified Data.Map                     as M
import qualified Data.Vector                  as V
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Control.Applicative
import           CPLEX.Bindings
import           CPLEX.Param
import           Data.Char(ord)
import           Unsafe.Coerce

newtype CpxEnv = CpxEnv (Ptr CpxEnv')
newtype CpxLp = CpxLp (Ptr CpxLp')

data ObjSense = CPX_MIN | CPX_MAX deriving Show
instance Enum ObjSense where
  fromEnum CPX_MIN = 1
  fromEnum CPX_MAX = -1
  toEnum 1 = CPX_MIN
  toEnum (-1) = CPX_MAX
  toEnum k = error $ "ObjSense: toEnum: unhandled value: " ++ show k

data Sense = L Double
           | E Double
           | G Double
           | R Double Double
           deriving (Eq, Show)

data Bound = L' Double
           | E' Double
           | U' Double
           deriving (Eq, Show)

data Type = CPX_CONTINUOUS
          | CPX_BINARY
          | CPX_INTEGER
          deriving (Eq, Show)

cpx_INFBOUND :: CDouble
cpx_INFBOUND = 1.1e20

newtype CpxRet = CpxRet CInt

getErrorString :: CpxEnv -> CpxRet -> IO String
getErrorString (CpxEnv env) (CpxRet status) = do
  msgPtr <- mallocArray 4096
  _ <- c_CPXgeterrorstring env status msgPtr
  msg <- peekCString msgPtr
  free msgPtr
  return msg

getStatString :: CpxEnv -> CpxRet -> IO String
getStatString (CpxEnv env) (CpxRet status) = do
  msgPtr <- mallocArray 510
  _ <- c_CPXgetstatstring env status msgPtr
  msg <- peekCString msgPtr
  free msgPtr
  return msg


openCPLEX :: IO (Either String CpxEnv)
openCPLEX = do
  statusPtr <- malloc
  env <- c_CPXopenCPLEX statusPtr
  status <- peek statusPtr
  free statusPtr

  if env == nullPtr
    then do
      -- env is NULL, throw error
      msg <- getErrorString (CpxEnv env) (CpxRet status)
      return (Left msg)
    else do
      return (Right (CpxEnv env))

closeCPLEX :: CpxEnv -> IO ()
closeCPLEX env@(CpxEnv env') = do
  -- free env
  envPtr <- new env'
  status <- c_CPXcloseCPLEX envPtr
  free envPtr
  case status of
    -- closed successfully
    0 -> do
      return ()
    -- close failed, print error message
    k -> do
      msg <- getErrorString env (CpxRet k)
      error $ "error calling CPXcloseCPLEX: " ++ msg

createProb :: CpxEnv -> String -> IO (Either String CpxLp)
createProb env@(CpxEnv env') name = do
  statusPtr <- malloc
  namePtr <- newCString name
  lp <- c_CPXcreateprob env' statusPtr namePtr
  status <- peek statusPtr
  free statusPtr
  free namePtr

  if lp == nullPtr
    then do
      -- lp is NULL, return error message
      msg <- getErrorString env (CpxRet status)
      return (Left msg)
    else return (Right (CpxLp lp))


freeProb :: CpxEnv -> CpxLp -> IO ()
freeProb env@(CpxEnv env') (CpxLp lp) = do
  -- free env
  lpPtr <- new lp
  status <- c_CPXfreeprob env' lpPtr
  free lpPtr

  case status of
    -- freed successfully
    0 -> return ()
    -- freeing failed, print error message
    k -> do
      msg <- getErrorString env (CpxRet k)
      error $ "error calling CPXfreeprob: " ++ msg


getNumCols :: CpxEnv -> CpxLp -> IO Int
getNumCols (CpxEnv env) (CpxLp lp) = fmap fromIntegral (c_CPXgetnumcols env lp)

getNumRows :: CpxEnv -> CpxLp -> IO Int
getNumRows (CpxEnv env) (CpxLp lp) = fmap fromIntegral (c_CPXgetnumrows env lp)

data CpxSolution = CpxSolution { solObj   :: Double
                               , solStat  :: CPX_SOLUTION_CODE
                               , solX     :: Vector Double
                               , solPi    :: Vector Double
                               , solSlack :: Vector Double
                               , solDj    :: Vector Double
                               } deriving Show

getSolution :: CpxEnv -> CpxLp -> IO (Either String  CpxSolution)
getSolution env@(CpxEnv env') lp@(CpxLp lp') = do
  lpstat' <- malloc
  objval' <- malloc

  numrows <- getNumRows env lp
  numcols <- getNumCols env lp
  x <- VSM.new numcols
  p <- VSM.new numrows
  slack <- VSM.new numrows
  dj <- VSM.new numcols

  status <-
    VSM.unsafeWith x $ \x' ->
    VSM.unsafeWith p $ \p' ->
    VSM.unsafeWith slack $ \slack' ->
    VSM.unsafeWith dj $ \dj' ->
    c_CPXsolution env' lp' lpstat' objval' x' p' slack' dj'

  lpstat <- peek lpstat'
  objval <- peek objval'
  free lpstat'
  free objval'

  x'' <- VS.freeze x
  p'' <- VS.freeze p
  slack'' <- VS.freeze slack
  dj'' <- VS.freeze dj

  case status of
    0 -> do
      statString <- getStatString env (CpxRet lpstat)

--      filename <- newCAString "lp.lp"
--      status <- c_CPXwriteprob env' lp' filename nullPtr

      return $ Right $ CpxSolution { solObj = realToFrac objval
                                   , solStat = intToSolution lpstat
                                   , solX = VS.map realToFrac x''
                                   , solPi = VS.map realToFrac p''
                                   , solSlack = VS.map realToFrac slack''
                                   , solDj = VS.map realToFrac dj''
                                   }
    k -> fmap Left (getErrorString env (CpxRet k))





getMIPSolution :: CpxEnv -> CpxLp -> IO (Either String  CpxSolution)
getMIPSolution env@(CpxEnv env') lp@(CpxLp lp') = do
  lpstat' <- malloc
  objval' <- malloc

  numrows <- getNumRows env lp
  numcols <- getNumCols env lp
  x <- VSM.new numcols

  status <-
    VSM.unsafeWith x $ \x' ->
    c_CPXsolution env' lp' lpstat' objval' x' nullPtr nullPtr nullPtr

  lpstat <- peek lpstat'
  objval <- peek objval'
  free lpstat'
  free objval'

  x'' <- VS.freeze x
  case status of
    0 -> do
      statString <- getStatString env (CpxRet lpstat)

      return $ Right $ CpxSolution { solObj = realToFrac objval
                                   , solStat = intToSolution lpstat
                                   , solX = VS.map realToFrac x''
                                   , solPi = VS.empty
                                   , solSlack = VS.empty 
                                   , solDj = VS.empty 
                                   }
    k -> fmap Left (getErrorString env (CpxRet k))





getMIPSolution' :: CpxEnv -> CpxLp -> IO (Either String CpxSolution)
getMIPSolution' env@(CpxEnv env') lp@(CpxLp lp') = do
  status <- c_CPXchgprobtype env' lp' $ typeToInt CPX_PROB_MILP
  case status of
    0 -> do
      objval' <- malloc

      numrows <- getNumRows env lp
      numcols <- getNumCols env lp
      x <- VSM.new numcols
      slack <- VSM.new numrows

      let statString = ""

      status <- c_CPXgetobjval env' lp' objval'
      case status of
        0 -> do
          objval <- peek objval'
          status <- VSM.unsafeWith x $ \x' ->
            c_CPXgetx env' lp' x' 0 $ fromIntegral (numcols - 1)
          case status of
            0 -> do
              x'' <- VS.freeze x
              status <- VSM.unsafeWith slack $ \slack' ->
                c_CPXgetslack env' lp' slack' 0 $ fromIntegral (numrows - 1)
              case status of
                0 -> do
                  slack'' <- VS.freeze slack


                  return $ Right $ CpxSolution { solObj = realToFrac objval
                                               , solStat = intToSolution 0
                                               , solX = VS.map realToFrac x''
                                               , solPi = VS.fromList $ take numcols [0.0,0.0..]
                                               , solSlack = VS.map realToFrac slack''
                                               , solDj = VS.fromList $ take numcols [0.0,0.0..]
                                               }

            k -> fmap Left (getErrorString env (CpxRet k))
        k -> fmap Left (getErrorString env (CpxRet k))
    k -> fmap Left (getErrorString env (CpxRet k))


getMipRelGap :: CpxEnv -> Ptr () -> CInt -> IO Double
getMipRelGap (CpxEnv env') cbdata wherefrom = do
  gap_p :: Ptr CDouble <- malloc
  status <- c_CPXgetcallbackinfo env' cbdata wherefrom 125 (unsafeCoerce gap_p)
  objVal :: CDouble <- peek gap_p 
  free gap_p
  return $ if status == 0 then realToFrac objVal else fromIntegral status

getMipBestInteger :: CpxEnv -> Ptr () -> CInt -> IO Double
getMipBestInteger (CpxEnv env') cbdata wherefrom = do
  gap_p :: Ptr CDouble <- malloc
  status <- c_CPXgetcallbackinfo env' cbdata wherefrom 101 (unsafeCoerce gap_p)
  objVal :: CDouble <- peek gap_p 
  free gap_p
  return $ if status == 0 then realToFrac objVal else fromIntegral status

writeprob :: CpxEnv -> CpxLp -> String -> IO (Maybe String)
writeprob env@(CpxEnv env') lp@(CpxLp lp') filename = do
  fn <- newCAString filename 
  status <- c_CPXwriteprob env' lp' fn nullPtr
  getErrorStatus env status

getBaseVars :: CpxEnv -> CpxLp -> IO (Maybe (Vector Int))
getBaseVars env@(CpxEnv env') lp@(CpxLp lp') = do
  numcols <- getNumCols env lp
  x <- VSM.new numcols
  VSM.unsafeWith x $ \x' -> do
    status <- c_CPXgetbase  env' lp' x' nullPtr
    case status of
      0 -> do vec <- VS.freeze x
              let vec' = VS.map fromIntegral vec
              return $ Just $ vec'
      _ -> return Nothing

toCpxError :: CpxEnv -> CInt -> IO (Maybe String)
toCpxError env 0 = return Nothing
toCpxError env k = do
  msg <- liftIO $ getErrorString env (CpxRet k)
  return $ Just msg

setIntParam :: CpxEnv -> CPX_PARAM -> CInt -> IO ()
setIntParam env@(CpxEnv env') param val = do
  status <- c_CPXsetintparam env' (paramToInt param) val
  case status of
    0 -> return ()
    k -> do
      putStrLn $ "CPXsetintparam failure setting " ++ show param
      msg <- getErrorString env (CpxRet k)
      error $ "error calling CPXsetintparam: " ++ msg

setDoubleParam :: CpxEnv -> CPX_PARAM -> CDouble -> IO ()
setDoubleParam env@(CpxEnv env') param val = do
  status <- c_CPXsetdblparam env' (paramToInt param) val
  case status of
    0 -> return ()
    k -> do
      putStrLn $ "CPXsetdblparam failure setting " ++ show param
      msg <- getErrorString env (CpxRet k)
      error $ "error calling CPXsetdblparam: " ++ msg

newtype Row = Row {unRow :: Int} deriving (Ord, Eq, Num)
newtype Col = Col {unCol :: Int} deriving (Ord, Eq, Num)
instance Show Row where
  show = show . unRow
instance Show Col where
  show = show . unCol

-- for use with the adding of rows, cuts or lazy constraints
toColForm' :: [(Row,Col,Double)] -> (Vector CInt, Vector CInt, Vector CDouble)
toColForm' amat = (VS.fromList matbeg, VS.fromList matind, VS.fromList matval)
    where
        cols = map (\(Row a, Col b, v) -> b) amat
        rows = map (\(Row a, _, _) -> a) amat
        numRows = maximum rows - (minimum rows) + 1
        numCols = maximum cols - (minimum cols) + 1
        rows' = map (\q -> filter (\(Row a, Col b, v) -> a== q ) amat) [minimum rows..maximum rows]
        matval = map (\(Row a, Col b, v) -> realToFrac v) $ concat rows'
        matind = map (\(Row a, Col b, v) -> fromIntegral b) $ concat rows'
        matbeg = map (fromIntegral . (*numCols)) [0..numRows-1]

toColForm :: Int -> [(Row,Col,Double)] -> (Vector CInt, Vector CInt, Vector CInt, Vector CDouble)
toColForm numcols amat = (matbeg, matcnt, matind, matval)
  where
    matbeg = VS.fromList $ map fromIntegral begs
    matcnt = VS.fromList $ map fromIntegral cnts
    matind = VS.fromList $ map (fromIntegral . unRow) inds
    matval = VS.fromList $ map realToFrac vals

    -- sort colMap into the from CPLEX wants
    inds :: [Row]
    vals :: [Double]
    (inds,vals) = unzip $ concat rows

    begs :: [Int]
    cnts :: [Int]
    rows :: [[(Row,Double)]]
    (begs,cnts,rows) = unzip3 $ colMapInfo' 0 $ M.elems colMap

    colMapInfo' :: Int -> [[(Row,Double)]] -> [(Int,Int,[(Row,Double)])]
    colMapInfo' beg (row:xs) = (beg,cnt,row) : colMapInfo' (beg + cnt) xs
      where
        cnt = length row
    colMapInfo' _ [] = []

    -- add Columns with no entries in case some are missing
    colMap = M.union colMap' emptyColMap

    emptyColMap :: M.Map Col [(Row,Double)]
    emptyColMap = M.fromList $ take numcols $ zip (map Col [0..]) (repeat [])

    -- a map from Col to all (Row,Double) pairs
    colMap' :: M.Map Col [(Row,Double)]
    colMap' = M.fromListWith (++) preorder

    -- reorganize the (Row,Col,Double) into (Col, [(Row,Double)]) with only 1 (Row,Double)
    preorder :: [(Col,[(Row,Double)])]
    preorder = map (\(row,col,val) -> (col, [(row, val)])) amat


toRowForm :: Int -> [(Row,Col,Double)] -> (Vector CInt, Vector CInt, Vector CInt, Vector CDouble)
toRowForm numrows amat = (matbeg, matcnt, matind, matval)
  where
    matbeg = VS.fromList $ map fromIntegral begs
    matcnt = VS.fromList $ map fromIntegral cnts
    matind = VS.fromList $ map (fromIntegral . unCol) inds
    matval = VS.fromList $ map realToFrac vals

    -- sort colMap into the from CPLEX wants
    inds :: [Col]
    vals :: [Double]
    (inds,vals) = unzip $ concat cols 

    begs :: [Int]
    cnts :: [Int]
    cols :: [[(Col,Double)]]
    (begs,cnts,cols) = unzip3 $ rowMapInfo' 0 $ M.elems rowMap

    rowMapInfo' :: Int -> [[(Col,Double)]] -> [(Int,Int,[(Col,Double)])]
    rowMapInfo' beg (col:xs) = (beg,cnt,col) : rowMapInfo' (beg + cnt) xs
      where
        cnt = length col 
    rowMapInfo' _ [] = []

    -- add Rows with no entries in case some are missing
    rowMap = M.union rowMap' emptyRowMap

    emptyRowMap :: M.Map Row [(Col,Double)]
    emptyRowMap = M.fromList $ take numrows $ zip (map Row [0..]) (repeat [])

    -- a map from Row to all (Col,Double) pairs
    rowMap' :: M.Map Row [(Col,Double)]
    rowMap' = M.fromListWith (++) preorder

    -- reorganize the (Row,Col,Double) into (Row, [(Col,Double)]) with only 1 (Row,Double)
    preorder :: [(Row,[(Col,Double)])]
    preorder = map (\(row,col,val) -> (row, [(col, val)])) amat

copyLp :: CpxEnv -> CpxLp -> ObjSense -> V.Vector Double -> V.Vector Sense -> [(Row,Col,Double)] -> V.Vector (Maybe Double, Maybe Double) -> IO (Maybe String)
copyLp = copyLpWithFun' c_CPXcopylp

--checkCopyLp :: CpxEnv -> CpxLp -> ObjSense -> V.Vector Double -> V.Vector Sense -> [(Row,Col,Double)] -> V.Vector (Maybe Double, Maybe Double) -> IO (Maybe String)
--checkCopyLp = copyLpWithFun' c_CPXcheckcopylp

type CopyLp =
  Ptr CpxEnv' -> Ptr CpxLp' -> CInt -> CInt -> CInt -> Ptr CDouble ->
  Ptr CDouble -> Ptr CChar -> Ptr CInt -> Ptr CInt ->
  Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

copyLpWithFun' :: CopyLp -> CpxEnv -> CpxLp -> ObjSense -> V.Vector Double -> V.Vector Sense -> [(Row,Col,Double)] -> V.Vector (Maybe Double, Maybe Double) -> IO (Maybe String)
copyLpWithFun' copyLpFun env lp objsense objcoeffs senseRhsRngVal aMat xbnds =
  copyLpWithFun copyLpFun env lp  numcols numrows objsense (VS.fromList (V.toList (V.map realToFrac objcoeffs))) rhs sense matbeg matcnt matind matval lb ub rngval
  where
    numcols = V.length objcoeffs -- or xbnds
    numrows = V.length senseRhsRngVal

    toBnds :: (Maybe Double, Maybe Double) -> (CDouble, CDouble)
    toBnds (Nothing, Nothing) = (-cpx_INFBOUND,  cpx_INFBOUND)
    toBnds ( Just x, Nothing) = ( realToFrac x,  cpx_INFBOUND)
    toBnds (Nothing,  Just y) = (-cpx_INFBOUND,  realToFrac y)
    toBnds ( Just x,  Just y) = ( realToFrac x,  realToFrac y)

    lb = VS.fromList $ V.toList lb'
    ub = VS.fromList $ V.toList ub'
    (lb',ub') = V.unzip $ V.map toBnds xbnds

    toRhs :: Sense -> (CChar, CDouble, CDouble)
    toRhs (L x)   = (castCharToCChar 'L', realToFrac x,               0)
    toRhs (E x)   = (castCharToCChar 'E', realToFrac x,               0)
    toRhs (G x)   = (castCharToCChar 'G', realToFrac x,               0)
    toRhs (R l u) = (castCharToCChar 'R', realToFrac l, realToFrac (u-l))

    sense  = VS.fromList $ V.toList sense'
    rngval = VS.fromList $ V.toList rngval'
    rhs    = VS.fromList $ V.toList rhs'
    (sense', rhs', rngval') = V.unzip3 $ V.map toRhs senseRhsRngVal

    (matbeg, matcnt, matind, matval) = toColForm numcols aMat


copyLpWithFun :: CopyLp -> CpxEnv -> CpxLp -> Int -> Int -> ObjSense -> Vector CDouble -> Vector CDouble -> Vector CChar -> Vector CInt -> Vector CInt -> Vector CInt -> Vector CDouble -> Vector CDouble -> Vector CDouble -> Vector CDouble -> IO (Maybe String)
copyLpWithFun copylpFun env@(CpxEnv env') (CpxLp lp) numcols numrows objsense obj rhs sense matbeg matcnt matind matval lb ub rngval = do
--  setIntParam env CPX_PARAM_SCRIND cpx_ON
--  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  let objsense' = fromIntegral (fromEnum objsense)
      numcols' = fromIntegral numcols
      numrows' = fromIntegral numrows
 -- print objsense
 -- print obj
 -- print rhs
 -- print sense
 -- print matbeg
 -- print matcnt
 -- print matind
 -- print matval
 -- print lb
 -- print ub
 -- print rngval
 -- print numrows'
 -- print numcols'
  status <-
    VS.unsafeWith obj    $ \obj' ->
    VS.unsafeWith rhs    $ \rhs' ->
    VS.unsafeWith sense  $ \sense' ->
    VS.unsafeWith matbeg $ \matbeg' ->
    VS.unsafeWith matcnt $ \matcnt' ->
    VS.unsafeWith matind $ \matind' ->
    VS.unsafeWith matval $ \matval' ->
    VS.unsafeWith lb     $ \lb' ->
    VS.unsafeWith ub     $ \ub' ->
    VS.unsafeWith rngval $ \rngval' ->
    copylpFun env' lp numcols' numrows' objsense' obj' rhs' sense' matbeg' matcnt' matind' matval' lb' ub' rngval'

  case status of
    0 -> return Nothing
    k -> Just <$> getErrorString env (CpxRet k)

type CopyQuad =
  Ptr CpxEnv' -> Ptr CpxLp' -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

copyQuad :: CpxEnv -> CpxLp -> [(Row,Col,Double)] -> IO (Maybe String)
copyQuad = copyQuadWithFun' c_CPXcopyquad

copyQuadWithFun' :: CopyQuad -> CpxEnv -> CpxLp -> [(Row,Col,Double)] -> IO (Maybe String)
copyQuadWithFun' copyQuadFun env lp aMat = do
  numcols <- getNumCols env lp
  let (matbeg, matcnt, matind, matval) = toColForm numcols aMat
  copyQuadWithFun copyQuadFun env lp  matbeg matcnt matind matval

copyQuadWithFun :: CopyQuad -> CpxEnv -> CpxLp -> Vector CInt -> Vector CInt -> Vector CInt -> Vector CDouble -> IO (Maybe String)
copyQuadWithFun copyQuadFun env@(CpxEnv env') (CpxLp lp) matbeg matcnt matind matval = do
  status <-
    VS.unsafeWith matbeg $ \matbeg' ->
    VS.unsafeWith matcnt $ \matcnt' ->
    VS.unsafeWith matind $ \matind' ->
    VS.unsafeWith matval $ \matval' ->
    copyQuadFun env' lp matbeg' matcnt' matind' matval'

  case status of
    0 -> return Nothing
    k -> fmap Just $ getErrorString env (CpxRet k)


copyMip :: CpxEnv -> CpxLp -> ObjSense -> V.Vector Double -> V.Vector Sense -> [(Row,Col,Double)] -> V.Vector (Maybe Double, Maybe Double) -> V.Vector Type -> IO (Maybe String)
copyMip env@(CpxEnv env') lp@(CpxLp lp') objsense objcoeffs senseRhsRngVal aMat xbnds ctypes = do
  status <- copyLpWithFun' c_CPXcopylp env lp objsense objcoeffs senseRhsRngVal aMat xbnds
  case status of
    Nothing -> do
         status' <-
           VS.unsafeWith ctypes' $ \ctypes'' ->
           c_CPXcopyctype env' lp' ctypes''

         case status' of
           0 -> return Nothing
           k' -> fmap Just $ getErrorString env (CpxRet k')


    Just msg -> return $ Just msg --fmap Just $ getErrorString env (CpxRet k)

  where
    toCType :: Type -> CChar
    toCType CPX_CONTINUOUS = castCharToCChar 'C'
    toCType CPX_BINARY     = castCharToCChar 'B'
    toCType CPX_INTEGER    = castCharToCChar 'I'

    ctypes' :: VS.Vector CChar
    ctypes' = VS.fromList $ map toCType $ V.toList ctypes


withOptFun :: (Ptr CpxEnv' -> Ptr CpxLp' -> IO CInt) -> CpxEnv -> CpxLp -> IO (Maybe String)
withOptFun optFun env@(CpxEnv env') (CpxLp lp') = do
  status <- optFun env' lp'
  case status of
    0 -> return Nothing
    k -> fmap Just (getErrorString env (CpxRet k))

qpopt :: CpxEnv -> CpxLp -> IO (Maybe String)
qpopt = withOptFun c_CPXqpopt

lpopt :: CpxEnv -> CpxLp -> IO (Maybe String)
lpopt = withOptFun c_CPXlpopt

mipopt :: CpxEnv -> CpxLp -> IO (Maybe String)
mipopt = withOptFun c_CPXmipopt

primopt :: CpxEnv -> CpxLp -> IO (Maybe String)
primopt = withOptFun c_CPXprimopt

dualopt :: CpxEnv -> CpxLp -> IO (Maybe String)
dualopt = withOptFun c_CPXdualopt

siftopt :: CpxEnv -> CpxLp -> IO (Maybe String)
siftopt = withOptFun c_CPXsiftopt

hybnetopt :: CpxEnv -> CpxLp -> Int -> IO (Maybe String)
hybnetopt env lp k = withOptFun (\e l -> c_CPXhybnetopt e l (fromIntegral k)) env lp

-------------------------------------------------


changeCoefList :: CpxEnv -> CpxLp -> V.Vector (Row, Col, Double) -> IO (Maybe String)
changeCoefList env@(CpxEnv env') (CpxLp lp') rcv = do
  let num = fromIntegral (V.length rcv)
      (rows', cols', vals') = V.unzip3 rcv
      rows = VS.fromList $ V.toList $ V.map (fromIntegral . unRow) rows'
      cols = VS.fromList $ V.toList $ V.map (fromIntegral . unCol) cols'
      vals = VS.fromList $ V.toList $ V.map realToFrac vals'
  status <-
    VS.unsafeWith rows $ \rows'' ->
    VS.unsafeWith cols $ \cols'' ->
    VS.unsafeWith vals $ \vals'' ->
    c_CPXchgcoeflist env' lp' num rows'' cols'' vals''
  case status of
    0 -> return Nothing
    k -> fmap Just $ getErrorString env (CpxRet k)


changeRhs :: CpxEnv -> CpxLp -> V.Vector (Row, Double) -> IO (Maybe String)
changeRhs env@(CpxEnv env') (CpxLp lp') rv = do
  let num = fromIntegral (V.length rv)
      (rows', vals') = V.unzip rv
      rows = VS.fromList $ V.toList $ V.map (fromIntegral . unRow) rows'
      vals = VS.fromList $ V.toList $ V.map realToFrac vals'
  status <-
    VS.unsafeWith rows $ \rows'' ->
    VS.unsafeWith vals $ \vals'' ->
    c_CPXchgrhs env' lp' num rows'' vals''
  case status of
    0 -> return Nothing
    k -> fmap Just $ getErrorString env (CpxRet k)

changeObj :: CpxEnv -> CpxLp -> V.Vector (Col, Double) -> IO (Maybe String)
changeObj env@(CpxEnv env') (CpxLp lp') cv = do
  let num = fromIntegral (V.length cv)
      (cols', vals') = V.unzip cv
      cols = VS.fromList $ V.toList $ V.map (fromIntegral . unCol) cols'
      vals = VS.fromList $ V.toList $ V.map realToFrac vals'
  status <-
    VS.unsafeWith cols $ \cols'' ->
    VS.unsafeWith vals $ \vals'' ->
    c_CPXchgobj env' lp' num cols'' vals''
  case status of
    0 -> return Nothing
    k -> fmap Just $ getErrorString env (CpxRet k)


changeBds :: CpxEnv -> CpxLp -> V.Vector (Col, Bound) -> IO (Maybe String)
changeBds env@(CpxEnv env') (CpxLp lp') cb = do
  let num = fromIntegral (V.length cb)
      (cols', bounds') = V.unzip cb

      fromBound :: Bound -> (CChar, CDouble)
      fromBound (L' x) = (castCharToCChar 'L', realToFrac x)
      fromBound (U' x) = (castCharToCChar 'U', realToFrac x)
      fromBound (E' x) = (castCharToCChar 'E', realToFrac x)

      (lus', bds') = V.unzip $ V.map fromBound bounds'

      cols = VS.fromList $ V.toList $ V.map (fromIntegral . unCol) cols'
      lus = VS.fromList $ V.toList $ lus'
      bds = VS.fromList $ V.toList $ bds'
  status <-
    VS.unsafeWith cols $ \cols'' ->
    VS.unsafeWith lus  $ \lus'' ->
    VS.unsafeWith bds  $ \bds'' ->
    c_CPXchgbds env' lp' num cols'' lus'' bds''
  case status of
    0 -> return Nothing
    k -> fmap Just $ getErrorString env (CpxRet k)

changeRngVal :: CpxEnv -> CpxLp -> V.Vector (Row, Double) -> IO (Maybe String)
changeRngVal env@(CpxEnv env') (CpxLp lp') rv = do
  let num = fromIntegral (V.length rv)
      (rows', vals') = V.unzip rv
      rows = VS.fromList $ V.toList $ V.map (fromIntegral . unRow) rows'
      vals = VS.fromList $ V.toList $ V.map realToFrac vals'
  status <-
    VS.unsafeWith rows $ \rows'' ->
    VS.unsafeWith vals $ \vals'' ->
    c_CPXchgrngval env' lp' num rows'' vals''
  case status of
    0 -> return Nothing
    k -> fmap Just $ getErrorString env (CpxRet k)

changeSens :: CpxEnv -> CpxLp -> V.Vector (Row, Char) -> IO (Maybe String)
changeSens env@(CpxEnv env') (CpxLp lp') rs = do
  let num = fromIntegral (V.length rs)
      (rows', senses') = V.unzip rs
      rows = VS.fromList $ V.toList $ V.map (fromIntegral . unRow) rows'
      senses = VS.fromList $ V.toList $ V.map castCharToCChar senses'
  status <-
    VS.unsafeWith rows $ \rows'' ->
    VS.unsafeWith senses $ \senses'' ->
    c_CPXchgsense env' lp' num rows'' senses''
  getErrorStatus env status

changeQpCoef :: CpxEnv -> CpxLp -> Row -> Col -> Double -> IO (Maybe String)
changeQpCoef env@(CpxEnv env') (CpxLp lp') (Row row) (Col col) val = do
  status <- c_CPXchgqpcoef env' lp' (fromIntegral row) (fromIntegral col) (realToFrac val)
  getErrorStatus env status


--------------------------------------------------

addCuts :: CpxEnv -> CpxLp -> Int -> V.Vector Sense -> [(Row, Col,Double)] -> IO (Maybe String)
addCuts env@(CpxEnv env') (CpxLp lp') nzcnt senseRhsRngVal aMat  = do
    status <-
        VS.unsafeWith rhs $ \rows'' ->
        VS.unsafeWith sense $ \senses'' ->
        VS.unsafeWith matbeg $ \matbeg'' ->
        VS.unsafeWith matind $ \matind'' ->
        VS.unsafeWith matval $ \matval'' ->

      c_CPXaddusercuts env' lp' (fromIntegral numrows) (fromIntegral nzcnt)
                rows'' senses'' matbeg'' matind'' matval'' nullPtr
    getErrorStatus env status
  where
    numrows = V.length senseRhsRngVal

    toRhs :: Sense -> (CChar, CDouble, CDouble)
    toRhs (L x)   = (castCharToCChar 'L', realToFrac x,               0)
    toRhs (E x)   = (castCharToCChar 'E', realToFrac x,               0)
    toRhs (G x)   = (castCharToCChar 'G', realToFrac x,               0)

    (sense', rhs', rngval') = V.unzip3 $ V.map toRhs senseRhsRngVal
    sense  = VS.fromList $ V.toList sense'
    rhs    = VS.fromList $ V.toList rhs'

    (matbeg, matind, matval) = toColForm' aMat


addLazyConstraints :: CpxEnv -> CpxLp -> Int -> V.Vector Sense -> [(Row, Col,Double)] -> IO (Maybe String)
addLazyConstraints env@(CpxEnv env') (CpxLp lp') nzcnt senseRhsRngVal aMat  = do
        status <-
            VS.unsafeWith rhs $ \rows'' ->
            VS.unsafeWith sense $ \senses'' ->
            VS.unsafeWith matbeg $ \matbeg'' ->
            VS.unsafeWith matind $ \matind'' ->
            VS.unsafeWith matval $ \matval'' ->

          c_CPXaddlazyconstraints env' lp' (fromIntegral numrows) (fromIntegral nzcnt)
                    rows'' senses'' matbeg'' matind'' matval'' nullPtr
        getErrorStatus env status
      where
        numrows = V.length senseRhsRngVal

        toRhs :: Sense -> (CChar, CDouble, CDouble)
        toRhs (L x)   = (castCharToCChar 'L', realToFrac x,               0)
        toRhs (E x)   = (castCharToCChar 'E', realToFrac x,               0)
        toRhs (G x)   = (castCharToCChar 'G', realToFrac x,               0)

        (sense', rhs', rngval') = V.unzip3 $ V.map toRhs senseRhsRngVal
        sense  = VS.fromList $ V.toList sense'
        rhs    = VS.fromList $ V.toList rhs'
        --matbeg = VS.fromList $ replicate numrows 0
        (matbeg, matind, matval) = toColForm' aMat


addCutFromCallback :: CpxEnv -> Ptr () -> CInt -> CInt -> Sense -> [(Col,Double)] -> CPX_CUT_TYPE -> IO (Maybe String)
addCutFromCallback env@(CpxEnv env') cbdata wherefrom nzcnt sense aMat purgable  = do
        status <-
            VS.unsafeWith cutind $ \ind ->
            VS.unsafeWith cutvals $ \vals ->
          c_CPXcutcallbackadd env' cbdata wherefrom nzcnt rhs' sense' ind vals purgable'
        getErrorStatus env status
      where
        cutind = VS.fromList $ map (\(Col a, v) -> fromIntegral a) aMat
        cutvals = VS.fromList $ map (\(Col a, v) -> realToFrac v) aMat
        purgable' = cutToInt purgable
        senseToInt (L x) = (fromIntegral $ ord 'L', realToFrac x)
        senseToInt (G x) = (fromIntegral $ ord 'G', realToFrac x)
        senseToInt (E x) = (fromIntegral $ ord 'E', realToFrac x)
        (sense', rhs') = senseToInt sense


addRows :: CpxEnv -> CpxLp -> Int -> Int -> Int -> V.Vector Sense -> [(Row, Col,Double)] -> IO (Maybe String)
addRows env@(CpxEnv env') (CpxLp lp') ccnt rcnt nzcnt senseRhsRngVal aMat  = do
    status <-
        VS.unsafeWith rhs $ \rows'' ->
        VS.unsafeWith sense $ \senses'' ->
        VS.unsafeWith matbeg $ \matbeg'' ->
        VS.unsafeWith matind $ \matind'' ->
        VS.unsafeWith matval $ \matval'' ->

      c_CPXaddrows env' lp' (fromIntegral ccnt) (fromIntegral rcnt) (fromIntegral nzcnt)
                rows'' senses'' matbeg'' matind'' matval'' nullPtr nullPtr
    getErrorStatus env status
  where
    numrows = V.length senseRhsRngVal

    toRhs :: Sense -> (CChar, CDouble, CDouble)
    toRhs (L x)   = (castCharToCChar 'L', realToFrac x,               0)
    toRhs (E x)   = (castCharToCChar 'E', realToFrac x,               0)
    toRhs (G x)   = (castCharToCChar 'G', realToFrac x,               0)

    (sense', rhs', rngval') = V.unzip3 $ V.map toRhs senseRhsRngVal
    sense  = VS.fromList $ V.toList sense'
    rhs    = VS.fromList $ V.toList rhs'

    (matbeg, matcnt, matind, matval) = toRowForm rcnt aMat

getErrorStatus env status = case status of
  0 -> return Nothing
  k -> Just <$> getErrorString env (CpxRet k)

addSingleMIPStart :: CpxEnv -> CpxLp -> [Int] -> [Double] -> CPX_EFFORT_LEVEL -> IO (Maybe String)
addSingleMIPStart env@(CpxEnv env') (CpxLp lp') indices values effortLevel = do
    let n = fromIntegral $ length indices
    status <- VS.unsafeWith (VS.fromList $ fromIntegral <$> indices) $ \indices' ->
              VS.unsafeWith (VS.fromList $ realToFrac <$> values) $ \values' ->
              VS.unsafeWith (VS.fromList [effortLevelToInt effortLevel]) $ \effortLevels ->
              VS.unsafeWith (VS.fromList [0]) $ \beg ->
              c_CPXaddmipstarts env' lp' 1 n beg indices' values' effortLevels nullPtr
    getErrorStatus env status


{-# NOINLINE setIncumbentCallback #-}
setIncumbentCallback :: CpxEnv -> CIncumbentCallback -> IO (Maybe String)
setIncumbentCallback env@(CpxEnv env') callback = do
    ptr <- c_createIncumbentCallbackPtr callback
    status <- c_CPXsetincumbentcallbackfunc env' ptr nullPtr
    getErrorStatus env status


{-# NOINLINE setCutCallback #-}
setCutCallback :: CpxEnv -> CCutCallback -> IO (Maybe String)
setCutCallback env@(CpxEnv env') callback = do
  ptr <- c_createCutCallbackPtr callback
  status <- c_CPXsetcutcallbackfunc env' ptr nullPtr
  getErrorStatus env status

{-# NOINLINE setLazyConstraintCallback #-}
setLazyConstraintCallback :: CpxEnv -> CCutCallback -> IO (Maybe String)
setLazyConstraintCallback env@(CpxEnv env') callback = do
  ptr <- c_createCutCallbackPtr callback
  status <- c_CPXsetlazyconstraintcallbackfunc env' ptr nullPtr
  getErrorStatus env status

getCallbackNodeX :: CpxEnv -> Ptr () -> Int -> Int -> Int -> IO (Maybe String, VS.Vector Double)
getCallbackNodeX env@(CpxEnv env') cbDataPtr wherefrom begin end = do
    xs' <- VSM.new (end - begin + 1)
    status <- VSM.unsafeWith xs' $ \ptr ->
                c_CPXgetcallbacknodex env' cbDataPtr (fromIntegral wherefrom) ptr (fromIntegral begin) (fromIntegral end)
    xs <- VS.freeze xs'
    stat <- getErrorStatus env status
    return (stat, VS.map realToFrac xs)

-- IMPORTANT FREE MEMORY AFTERWARDS
getCallbackLP :: CpxEnv -> Ptr () -> Int -> IO (Either String CpxLp)
getCallbackLP env@(CpxEnv env') cbDataPtr wherefrom = do
    lpPtrPtr <- mallocBytes 8
    lpstat <- c_CPXgetcallbacknodelp env' cbDataPtr (fromIntegral wherefrom) lpPtrPtr
    lpPtr <- peek lpPtrPtr
    free lpPtrPtr
    case lpstat of
                0 -> return $ Right $ CpxLp lpPtr
                k -> do statString <- getErrorString env (CpxRet lpstat)
                        return $ Left statString



-------------------------------------------------

withEnv :: (CpxEnv -> IO a) -> IO a
withEnv f = do
  env' <- openCPLEX
  case env' of
    Left msg -> error msg
    Right env -> do
      ret <- f env
      closeCPLEX env
      return ret

withLp :: CpxEnv -> String -> (CpxLp -> IO a) -> IO a
withLp env name f = do
  lp' <- createProb env name
  case lp' of
    Left msg -> error msg
    Right lp -> do
      ret <- f lp
      freeProb env lp
      return ret
