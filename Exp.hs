{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import           Control.Monad.Freer
import           Control.Monad.Freer.Internal

getInt :: String -> Int
getInt = fst . head . reads

data State
  = State { int :: Int, str :: String }
  deriving (Show)

data S s where
  SPut    :: String -> S ()
  SGet    :: S String
data I s where
  IPut    :: Int -> I ()
  IGet    :: I Int

sput' :: Member S r => String -> Eff r ()
sput' = send . SPut
sget'  :: Member S r => Eff r String
sget' = send SGet
iput' :: Member I r => Int -> Eff r ()
iput' = send . IPut
iget'  :: Member I r => Eff r Int
iget' = send IGet

foo :: (Member I r, Member S r) => Eff r String
foo = do
  sput' "lol"
  iput' 1
  x <- sget'
  return x

runS :: Eff '[S] w -> IO w
runS (Val x) = return x
runS (E u q) = case extract u of
                 SPut x -> do
                   putStrLn x
                   runS (qApp q ())
                 SGet   -> do
                   s <- getLine
                   runS (qApp q s)
runI :: Eff '[I] w -> IO w
runI (Val x) = return x
runI (E u q) = case extract u of
                 IPut x -> putStrLn (show x)  >>        runI (qApp q ())
                 IGet   -> getLine            >>= \i -> runI (qApp q (getInt i))

main = do
  -- ret <- runS . runI $ foo
  let ret = "a"
  putStrLn $ "win: " ++ ret 
