{-# LANGUAGE CPP #-}

module Main where

import Data.IORef
import SExpGen
import Test.QuickCheck
import Test.QuickCheck.Random
import System.Environment
import qualified Data.Map as M


seed :: QCGen
seed = read "SMGen 16900282403807365132 373647451104439981"

size = 66

#ifdef QUICKERCHECK
check = quickCheckPar
#else
check = quickCheck
#endif

properties = M.fromList
    [ ("prop_add", check (withMaxSuccess 100 prop_add)),
      ("prop_sub", check (withMaxSuccess 100 prop_sub)),
      ("prop_num_eq", check (withMaxSuccess 100 prop_num_eq)),
      ("prop_nil_program", check (withMaxSuccess 100 prop_nil_program)),
      ("prop_env", check (withMaxSuccess 100 prop_env)),
      ("prop_unflatten_flatten", check (withMaxSuccess 100 prop_unflatten_flatten)),
      ("prop_gc_progn", check (withMaxSuccess 100 prop_gc_progn)),
      ("prop_if_true",  check (withMaxSuccess 100 prop_if_true)),
      ("prop_if_false", check (withMaxSuccess 100 prop_if_false)),
      ("prop_cond",  check (withMaxSuccess 100 prop_cond)),
      ("prop_eval_quote", check (withMaxSuccess 100 prop_eval_quote)),
      ("prop_gc_2",check (withMaxSuccess 100 prop_gc_2)),
      ("prop_progn_step", check (withMaxSuccess 100 prop_progn_step)),
      ("prop_add_single", check (withMaxSuccess 100 prop_add_single)),
      ("prop_add_inductive", check (verboseShrinking $ withMaxSuccess 10000 prop_add_inductive)),
      ("prop_define", check (withMaxSuccess 1000 prop_define)),
      ("prop_lambda", check (withMaxSuccess 1000 prop_lambda)),
      ("prop_closure", check (withMaxSuccess 1000 prop_closure)),
      ("prop_progn", check (withMaxSuccess 1000 prop_progn)),
      ("prop_var", check (withMaxSuccess 1000 prop_var)),
      ("prop_let", check (withMaxSuccess 1000 prop_let)),
      ("prop_app", check (withMaxSuccess 1000 prop_app)),
      ("arb-ctx", putStrLn =<< (prettyCtx <$> generate genCtx)),
      ("arb-type", putStrLn =<< (prettyType <$> generate (arbSizedType 15))),
      ("arb-exp", putStrLn =<< (do t <- generate $ arbSizedType 15
                                   s <- generate $ chooseInt (1,10)
                                   (_, e) <- generate $ genExp newCtx s t
                                   return $ prettyExp e))
    ]
      

runProp :: String -> IO ()
runProp p_str = do
      let prop = M.lookup p_str properties
      case prop of
        (Just a) -> a
        Nothing -> putStrLn "Property not found"
  
main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  
  if (nargs > 0)
    then runProp (head args) 
    else putStrLn (unlines (M.keys properties))
    

