{-# LANGUAGE ForeignFunctionInterface #-}
module Termios where
import Foreign.C
import Foreign
 
foreign import ccall "set_icanon" set_icanon :: CInt -> IO ()
foreign import ccall "unset_icanon" unset_icanon :: CInt -> IO ()
foreign import ccall unsafe "rdtsc.h" rdtsc :: IO Word64
