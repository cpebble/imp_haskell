module ImpInterp where

import Control.Monad
import ImpAst

type Store = Location -> Constant

emptyStore :: Store
emptyStore = const 0

-- Should really use a shallow binding but here we are
assignStore :: Store -> Location -> Constant -> Store
assignStore s loc n loc' = if loc' == loc then n else s loc'

type Cmds = CExp

newtype Evaluator a = Eval {runEval :: Store -> (a, Store)}

instance Monad Evaluator where
  return a = Eval $ \s -> (a, s)
  m >>= f =
    Eval $ \store ->
      let (a, st1) = runEval m store
          b = runEval (f a) st1
       in b

instance (Show a) => Show (Evaluator a) where
  show m = show . fst $ runEval m emptyStore

-- You shouldn't need to modify these
instance Functor Evaluator where
  fmap = liftM

instance Applicative Evaluator where
  pure = return
  (<*>) = ap


getStore :: Evaluator Store
getStore = Eval $ \s -> (s, s)

addStoreElem :: Location -> Constant -> Evaluator ()
addStoreElem loc n = Eval $ \s -> ((), assignStore s loc n)

look :: Location -> Evaluator Constant
look loc = Eval $ \s -> (s loc, s)

evalAExp :: AExp -> Evaluator Constant
evalAExp (Const n) = return n
evalAExp (Loc l) = look l
evalAExp (Plus a0 a1) = do
  n0 <- evalAExp a0
  n1 <- evalAExp a1
  return $ n0 + n1
evalAExp (Minus ae ae') = do
  n0 <- evalAExp ae
  n1 <- evalAExp ae'
  return $ n0 - n1
evalAExp (Times ae ae') = do
  n0 <- evalAExp ae
  n1 <- evalAExp ae'
  return $ n0 * n1

evalBExp :: BExp -> Evaluator Boolean
evalBExp (Bool bo) = return bo
evalBExp (Eq ae ae') = do
  n0 <- evalAExp ae
  n1 <- evalAExp ae'
  return $ if n0 == n1 then ITrue else IFalse
evalBExp (Leq ae ae') = do
  n0 <- evalAExp ae
  n1 <- evalAExp ae'
  return $ if n0 <= n1 then ITrue else IFalse
evalBExp (Not be) = do
  b <- evalBExp be
  return $ case b of
    ITrue -> IFalse
    IFalse -> ITrue
evalBExp (And be be') = do
  t0 <- evalBExp be
  t1 <- evalBExp be'
  return $ if t0 == ITrue && t1 == ITrue then ITrue else IFalse

evalEvaluator :: Store -> Evaluator a -> a
evalEvaluator s m = fst $ runEval m s

evalCExp :: CExp -> Evaluator ()
evalCExp Skip = return ()
evalCExp (Assign loc ae) = 
  do
    n <- evalAExp ae
    addStoreElem loc n
evalCExp (Sequence ce0 ce1) = do
  evalCExp ce0
  evalCExp ce1
evalCExp (If be ce0 ce1) = do
    b <- evalBExp be
    if b == ITrue then
        evalCExp ce0
    else
        evalCExp ce1
evalCExp ce'@(While be ce0) = do
    b <- evalBExp be
    case b of
      ITrue -> do evalCExp ce0; evalCExp ce'
      IFalse -> return ()

