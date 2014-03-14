module ReaderAP (bacRead, nullAP) where

import qualified Data.ByteString as BS
import Data.Word

data Reader a = R (BS.ByteString -> (Maybe a, BS.ByteString))

bacRead :: Reader a -> BS.ByteString -> (Maybe a, BS.ByteString)
bacRead (R f) bs = f bs

success :: a -> Reader a
success a = R (\input -> (Just a, input))

failure :: Reader a
failure = R (\input -> (Nothing, input))

byte :: Reader Word8
byte = R (\input -> if BS.null input then (Nothing, BS.empty)
                    else (Just $ BS.head input, BS.tail input))

bindReader :: Reader a -> (a -> Reader b) -> Reader b
bindReader r f = R (\input -> case bacRead r input of
                                (Nothing, output) -> (Nothing, output)
                                (Just val, output) -> bacRead (f val) output)

instance Monad Reader where
  return = success
  (>>=) = bindReader

nullAP :: Reader ()
nullAP = do
  b <- byte
  if b == 0x00 then success () else failure
