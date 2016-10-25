{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Data.LP.Backend.Cplex(solLP, standardBounds, defaultCallBacks, getCallBackLp, getIncCallBackXs, getCallBackXs
                             ,addCallBackCut 
                             ,getCallBackGap 
                             ,getCallBackBestObjI 
                             ,solMIP
                             ,solMIP'
                             ,UserCutCallBack, CutCallBackM, UserIncumbentCallBack, IncumbentCallBackM, CallBacks(..)) where

import qualified Data.Vector as V
import CPLEX.Bindings
import CPLEX.Param
import CPLEX.Core hiding (Bound)
--import Foreign.C (CInt)
import Data.Internal
import qualified Data.LP as LP
import qualified Data.Vector.Storable as VS
import Foreign.Ptr
import Foreign.ForeignPtr(newForeignPtr_)
import Foreign.Storable
import Foreign.C
import Control.Monad
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.HashSet as S

data CallBacks a = ActiveCallBacks {cutcb :: Maybe (UserCutCallBack a), inccb :: Maybe (UserIncumbentCallBack),
                                  lazycb :: Maybe (UserCutCallBack a) }
defaultCallBacks :: CallBacks a
defaultCallBacks = ActiveCallBacks {cutcb = Nothing, inccb = Nothing, lazycb = Nothing}

type ParamValues = [(CPX_PARAM, Int)]

type VarDic a = Map a Int 
type RevDic a = Map Int a
data CutCallBackArgs a = CutCallBackArgs {env :: CpxEnv, cbdata :: Ptr (), wherefrom :: CInt, cbhandle :: Ptr (), userdata :: Ptr Int, vardic :: VarDic a, revdic :: RevDic a} 
type CutCallBackM b a = (ReaderT (CutCallBackArgs b) IO a) 
type UserCutCallBack a = CutCallBackM a Int 

data IncumbentCallBackArgs = IncumbentCallBackArgs {envi :: CpxEnv, cbdatai :: Ptr (), wherefromi :: CInt, cbhandlei :: Ptr (),
                                                    objVal :: CDouble, xs :: Ptr CDouble, isfeas :: Ptr Int , useraction :: Ptr Int} 
type IncumbentCallBackM a = (ReaderT IncumbentCallBackArgs IO a) 
type UserIncumbentCallBack = Double -> VS.Vector Double -> IncumbentCallBackM Bool

incumbentcallback :: UserIncumbentCallBack -> CIncumbentCallback
incumbentcallback usercb env' cbdata wherefrom cbhandle objVal xs isfeas useraction = do
    let env = CpxEnv env'
    let oval = realToFrac objVal
    foreignPtr <- newForeignPtr_ xs
    lp <- getCallbackLP env cbdata (fromIntegral wherefrom)
    xs' <- case lp of 
          Right lp' -> do 
                  colCount <- getNumCols env lp'
                  return $ VS.map realToFrac $ VS.unsafeFromForeignPtr0 foreignPtr colCount
          Left _ ->  return VS.empty
    isFeas <- runReaderT (usercb oval xs') $ IncumbentCallBackArgs env cbdata wherefrom cbhandle objVal xs isfeas useraction 
    poke isfeas (if isFeas then 1 else 0) 
    return 0

lazycallback :: (Eq a, Hashable a) => VarDic a -> RevDic a -> UserCutCallBack a -> CCutCallback
lazycallback vardic revdic usercb env' cbdata wherefrom cbhandle ptrUser = do
    let env = CpxEnv env'
    runReaderT usercb $ CutCallBackArgs env cbdata wherefrom cbhandle ptrUser vardic revdic

cutcallback :: (Eq a, Hashable a) => VarDic a -> RevDic a -> UserCutCallBack a -> CCutCallback
cutcallback = lazycallback

getCallBackLp :: (Eq a, Hashable a) => CutCallBackM a CpxLp
getCallBackLp = do
  CutCallBackArgs{..} <- ask
  res <- liftIO $ getCallbackLP env cbdata (fromIntegral wherefrom)
  case res of
    Right lp -> return lp
    _ -> error "I cant get the LP for some reason"

getIncCallBackXs :: IncumbentCallBackM (V.Vector Double)
getIncCallBackXs = do
    IncumbentCallBackArgs{..} <- ask
  
    lp <- liftIO $ getCallbackLP envi cbdatai (fromIntegral wherefromi)
    xs' <- case lp of 
              Right lp' -> do 
                  colCount <- liftIO $ getNumCols envi lp'
                  (stat, xsVS) <- liftIO $ getCallbackNodeX envi cbdatai (fromIntegral wherefromi) 0 (colCount-1)
                  return $ VS.convert xsVS
              Left msg -> return $ V.empty
    return xs'

getCallBackXs :: (Eq a, Hashable a) => CutCallBackM a (Map a Double)
getCallBackXs = do
    CutCallBackArgs{..} <- ask
  
    lp <- liftIO $ getCallbackLP env cbdata (fromIntegral wherefrom)
    xs' <- case lp of 
              Right lp' -> do 
                  colCount <- liftIO $ getNumCols env lp'
                  (stat, xsVS) <- liftIO $ getCallbackNodeX env cbdata (fromIntegral wherefrom) 0 (colCount-1)
                  return $ VS.convert xsVS
              Left msg -> return $ V.empty
    let vars = V.toList xs'
    let m = M.fromList $ zip (map (revdic M.!) [0..length vars - 1]) vars
    return m

getCallBackGap :: (Eq a, Hashable a) => CutCallBackM a Double
getCallBackGap = do
    CutCallBackArgs{..} <- ask
    gap <- liftIO $ getMipRelGap env cbdata wherefrom 
    return gap

getCallBackBestObjI :: (Eq a, Hashable a) => CutCallBackM a Double
getCallBackBestObjI = do
    CutCallBackArgs{..} <- ask
    gap <- liftIO $ getMipBestInteger env cbdata wherefrom 
    return gap

addCallBackCut :: (Eq a, Hashable a) => LP.Constraint a -> CutCallBackM a (Maybe String)
addCallBackCut st_ = do
    CutCallBackArgs{..} <- ask
    let st = tokenizeVars (LP.buildConstraint st_) vardic
    let (cnstrs,rhs) = toForm st
    liftIO $ addCutFromCallback env cbdata wherefrom (fromIntegral $ length cnstrs) rhs cnstrs CPX_USECUT_FORCE
  where
    toForm (vars :< b) = (map (\(v :# i) -> (Col i, v)) vars, L b)  
    toForm (vars :> b) = (map (\(v :# i) -> (Col i, v)) vars, G b)  
    toForm (vars := b) = (map (\(v :# i) -> (Col i, v)) vars, E b) 

standardBounds :: (Int, Int) -> [(Int, Maybe Double, Maybe Double)]
standardBounds (i,j) = [(i',Just 0, Nothing) | i' <- [i..j] ]

toBounds :: [(Int, Maybe Double, Maybe Double)] -> (Int, Int) -> V.Vector (Maybe Double, Maybe Double)
toBounds bounds vr@(a,b) = def V.// (map (\(q,w,e) -> (q,(w,e))) bounds)
    where
        def = V.fromList [k | _ <- [a..b], let k = (Just 0, Nothing)]

toConstraints :: Constraints Int -> (Int, Int) -> ([(Row, Col, Double)], V.Vector Sense)
toConstraints constraints varRange = let (st, rhs) = toStandard constraints 0 [] [] varRange
    in (st, V.fromList rhs)


toStandard :: Constraints Int -> Int -> [(Row, Col, Double)] -> [Sense] -> (Int, Int)
                    -> ([(Row, Col, Double)], [Sense])
toStandard (Constraints []) _ accSt accRhs _ = (reverse $ accSt, reverse $ accRhs)
toStandard (Constraints (b:bs)) rowI accSt accRhs varRange = case b of
        vars :< boundVal -> addRow vars L boundVal
        vars := boundVal -> addRow vars E boundVal
        vars :> boundVal -> addRow vars G boundVal
    where   addRow vars s boundVal = toStandard (Constraints bs) (rowI+1) (generateRow vars ++ accSt)
                                                ((s boundVal) : accRhs) varRange
            generateRow [] = []
            generateRow ((v :# i):vs) = (Row rowI, Col i, v):generateRow vs


toObj :: Optimization Int -> (ObjSense, V.Vector Double)
toObj (Maximize dd) = (CPX_MAX, varsToVector dd)
toObj (Minimize dd) = (CPX_MIN, varsToVector dd)

-- This assumes that all elements are in !, zero pad boys
varsToVector :: [Variable Int] -> V.Vector Double
varsToVector vs = V.fromList $ map snd $ sortBy (comparing fst) $ map (\(c :# i) -> (i,c)) vs


solLP :: (Eq a, Hashable a) => LP.LinearProblem a -> ParamValues -> IO (LPSolution a)
solLP (LP.LP objective__ constraints__ bounds_) params = withEnv $ \env -> do
  --setIntParam env CPX_PARAM_SCRIND cpx_ON
  --setIntParam env CPX_PARAM_DATACHECK cpx_ON
  mapM_ (\(p,v) -> setIntParam env p (fromIntegral v)) params
  withLp env "testprob" $ \lp -> do
    let
        constraints_ = LP.buildConstraints constraints__
        objective_ = LP.buildObjective objective__
        dic = generateVarDic constraints_ objective_ bounds_
        revDic = M.fromList $ map (\(a,b) -> (b,a)) $ M.toList dic
        objective = tokenizeObj objective_ dic
        constraints = tokenizeConstraints constraints_ dic
        bounds = tokenizeBounds bounds_ dic
        varRange = (0,M.size dic - 1)

        (varA, varB) = varRange
        varCount = varB - varA + 1
        (objsen, obj) = toObj objective
        (cnstrs,rhs) = toConstraints constraints varRange
        xbnds = toBounds bounds varRange
   
    statusLp <- copyLp env lp objsen obj rhs cnstrs xbnds

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    statusOpt <- qpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg

    statusSol <- getSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do 
          let vars = V.toList $ VS.convert $ solX sol 
          let m = M.fromList $ zip (map (revDic M.!) [0..length vars - 1]) vars
          basism <- getBaseVars env lp
          let basis' = basism >>= \basis -> Just $ S.fromList $ map (\(i,c) -> revDic M.! i) $ filter(\(i,c) -> c == 1) $  zip [0..] $ V.toList $ VS.convert basis
          return $ LPSolution (solStat sol == CPX_STAT_OPTIMAL) (solObj sol) ( m ) (VS.convert $ solPi sol) basis'

solMIP :: (Eq a, Hashable a) => LP.MixedIntegerProblem a -> ParamValues -> CallBacks a -> IO (MIPSolution a)
solMIP = solMIP' M.empty
solMIP' :: (Eq a, Hashable a) => Map a Double -> LP.MixedIntegerProblem a -> ParamValues -> CallBacks a -> IO (MIPSolution a)
solMIP' warmStart (LP.MILP objective__ constraints__ bounds_ types_ ) params (ActiveCallBacks {..})  = withEnv $ \env -> do
--  setIntParam env CPX_PARAM_SCRIND 1
 -- setIntParam env CPX_PARAM_DATACHECK 1 
  mapM_ (\(p,v) -> setIntParam env p (fromIntegral v)) params
  withLp env "clu" $ \lp -> do
    let
        constraints_ = LP.buildConstraints constraints__
        objective_ = LP.buildObjective objective__
        dic = generateVarDic constraints_ objective_ bounds_
        revDic = M.fromList $ map (\(a,b) -> (b,a)) $ M.toList dic
        objective = tokenizeObj objective_ dic
        constraints = tokenizeConstraints constraints_ dic
        bounds = tokenizeBounds bounds_ dic
        types = tokenizeTypes types_ dic
        varRange = (0,M.size dic - 1)

        (varA, varB) = varRange
        varCount = varB - varA + 1
        (objsen, obj) = toObj objective
        (cnstrs,rhs) = toConstraints constraints varRange
        xbnds = toBounds bounds varRange
        
        types' = V.fromList (replicate varCount CPX_CONTINUOUS) V.// (map (\(a,t) -> (a, typeToCPX t)) types)
    
    statusLp <- copyMip env lp objsen obj rhs cnstrs xbnds types' 

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopyMIP error: " ++ msg

    case inccb of 
        Just cb -> do   statusIncCB <- setIncumbentCallback env (incumbentcallback cb)
                        case statusIncCB of
                            Nothing -> return ()
                            Just msg -> error $ "CPXIncumbentCallBackSet Error: " ++ msg
        Nothing -> return ()
    
    case lazycb of 
        Just cb -> do   statusLazyCB <- setLazyConstraintCallback env (lazycallback dic revDic cb)
                        case statusLazyCB of
                            Nothing -> return ()
                            Just msg -> error $ "CPXLazyConstraintCallBackSet Error: " ++ msg
        Nothing -> return ()

    case cutcb of 
        Just cb -> do   statusCutCB <- setCutCallback env (cutcallback dic revDic cb)
                        case statusCutCB of
                            Nothing -> return ()
                            Just msg -> error $ "CPXCutCallBackSet Error: " ++ msg
        Nothing -> return ()

    let warmVars = M.toList warmStart
    let warmInd = map (dic M.!) $ map fst warmVars
    let warmVals = map snd warmVars
    when (M.size warmStart > 0) $ addSingleMIPStart env lp warmInd warmVals CPX_MIPSTART_AUTO >> return ()

    statusOpt <- mipopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXmipopt error: " ++ msg

    statusSol <- getMIPSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do -- I call this imperative do notation 
          let vars = V.toList $ VS.convert $ solX sol 
          let m = M.fromList $ zip (map (revDic M.!) [0..length vars - 1]) vars
          return $ MIPSolution (solStat sol == CPXMIP_OPTIMAL) (solObj sol) m 
       
typeToCPX :: Data.Internal.Type -> CPLEX.Core.Type 
typeToCPX (TInteger) = CPX_INTEGER
typeToCPX (TContinuous) = CPX_CONTINUOUS
typeToCPX (TBinary) = CPX_BINARY


generateVarDic :: (Eq a, Hashable a) => Constraints a -> Optimization a -> [(a, Maybe Double, Maybe Double)]
                                    -> VarDic a
generateVarDic (Constraints bounds) opt bs = let allvars = (concat $ map getBounds bounds) ++ (getOptim opt) ++ (map fst' bs)
                                             in go allvars 0 M.empty
  where
    getVar (_ :# v) = v
    getBounds (vs :< _ ) = map getVar vs
    getBounds (vs :> _ ) = map getVar vs
    getBounds (vs := _ ) = map getVar vs
    getOptim (Maximize a) = map getVar a
    getOptim (Minimize a) = map getVar a
    fst' (a,b,c) = a
    go :: (Eq a, Hashable a) => [a] -> Int -> Map a Int -> Map a Int
    go [] i m = m
    go (v:vs) i m 
      | v `M.member` m = go vs i m
      | otherwise = go vs (i+1) $ M.insertWith (\new old -> old) v i m

-- Change variables in bound to have ID
tokenizeConstraints :: (Eq a, Hashable a) => Constraints a -> Map a Int -> Constraints Int
tokenizeConstraints (Constraints bounds) dic = Constraints $ map b2b bounds
  where
    v2v (d :# v) = d :# (dic M.! v)
    b2b (vs :< d) = map v2v vs :< d
    b2b (vs :> d) = map v2v vs :> d
    b2b (vs := d) = map v2v vs := d

tokenizeVars :: (Eq a, Hashable a) => Bound [Variable a] -> Map a Int -> Bound [Variable Int]
tokenizeVars bounds dic = b2b bounds
  where
    v2v (d :# v) = d :# (dic M.! v)
    b2b (vs :< d) = map v2v vs :< d
    b2b (vs :> d) = map v2v vs :> d
    b2b (vs := d) = map v2v vs := d

tokenizeObj :: (Eq a, Hashable a) => Optimization a -> Map a Int -> Optimization Int 
tokenizeObj obj dic = o2o obj
  where
    v2v (d :# v) = d :# (dic M.! v)
    o2o (Minimize vs) = Minimize $ map v2v vs
    o2o (Maximize vs) = Maximize $ map v2v vs

tokenizeBounds :: (Eq a, Hashable a) => [(a,b,b)] -> Map a Int -> [(Int,b,b)]
tokenizeBounds xs dic = map (\(a,b,c) -> (dic M.! a, b, c)  ) xs

tokenizeTypes :: (Eq a, Hashable a) => [(a,b)] -> Map a Int -> [(Int, b)]
tokenizeTypes xs dic = map (\(a,b) -> (dic M.! a, b)  ) xs
