module Debug where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)

{-# NOINLINE debugFlag #-}
debugFlag :: IORef Bool
debugFlag = unsafePerformIO $ newIORef False

setDebug :: Bool -> IO ()
setDebug b = writeIORef debugFlag b

isDebug :: Bool
isDebug = unsafePerformIO $ readIORef debugFlag

trace :: String -> a -> a
trace msg x = if isDebug then trace' msg x else x
  where
    trace' msg x = unsafePerformIO $ do
        putStrLn (take 1000 msg)
        putStrLn ""
        return x 
    --   if length msg < 200 
    --     then do 
    --         putStrLn msg
    --         putStrLn ""
    --         putStrLn ""
    --     else 
    --         print ""
    --   return x 