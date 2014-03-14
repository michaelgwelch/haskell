module ReaderAP (bacRead, nullAP) where

import qualified Data.ByteString as BS

data Reader a = R (BS.ByteString -> (Maybe a, BS.ByteString))

bacRead :: Reader a -> BS.ByteString -> (Maybe a, BS.ByteString)
bacRead (R f) bs = f bs

nullAP :: Reader ()
nullAP = R (\input -> if (BS.head input) == 0x00 then (Just (), BS.tail input)
                      else (Nothing, BS.tail input))
