module Reader where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32)
import Data.Bits ((.&.),shiftR,testBit)
data Reader a = R (BS.ByteString -> Maybe (a,BS.ByteString))

-- Read a byte

readit :: Reader a -> BS.ByteString -> Maybe (a, BS.ByteString)
readit (R r) bs = r bs

readit' :: Reader a -> BS.ByteString -> a
readit' r bs = result
               where Just(result, _) = readit r bs

-- | Given the initial octet (which contains the lvt value)
--   this method determines the length of the content. This
--   method will consume length bytes as necessary (i.e. the 
--   lvt value is B101 or 5)
readLength :: Word8 -> Reader Word16
readLength b = error "not implemented"

success :: a -> Reader a
success a = R (\input -> Just(a,input))

failure :: Reader a
failure = R (\input -> Nothing)

byte :: Reader Word8
byte = R (\input -> if BS.null input then Nothing 
                    else Just(BS.head input, BS.tail input))

bytes :: Word8 -> Reader [Word8]
bytes 0 = success []
bytes count = error "not implemented"
-- bytes count = byte >>= \b ->
--              return (b : bytes (count - 1))

bindReader :: Reader a -> (a -> Reader b) -> Reader b
bindReader r f = R (\input -> case readit r input of
                                Nothing -> Nothing
                                Just(b, out) -> readit (f b) out)
                      
tagNumber :: Word8 -> Word8
tagNumber b = shiftR b 4 .&. 0x0F

isApplication :: Word8 -> Bool
isApplication = not . (flip testBit) 3

tagNull :: Word8
tagNull = 0

tagBool :: Word8
tagBool = 1

tagUnsigned :: Word8
tagUnsigned = 2


assertTag :: Word8 -> Word8 -> Reader ()
assertTag b tagNum = if tagNumber b == tagNum then success ()
                     else failure

assertApplication :: Word8 -> Reader ()
assertApplication b = if isApplication b then success () else failure

assertContextSpecific :: Word8 -> Reader ()
assertContextSpecific b = if isContextSpecific b then success () else failure

assertLength :: Word8 -> Word8 -> Reader ()
assertLength b l = if lvt b == l then success () else failure

isContextSpecific :: Word8 -> Bool
isContextSpecific = not . isApplication

lvt :: Word8 -> Word8
lvt = (.&. 0x07)

closeType :: Word8
closeType = 0x07

openType :: Word8
openType = 0x06


item :: Word8 -> Reader ()
item b = byte >>= \inp ->
         if b == inp then success () else failure

--  Monad Reader

instance Monad Reader where
   return x = success x
   r >>= f = bindReader r f

-- Null

readNullAP :: Reader ()
readNullAP = item 0x00

readNullCS :: Word8 -> Reader ()
readNullCS tag = byte >>= \b ->
                 assertTag b tag >>
                 assertContextSpecific b >>
                 assertLength b 0

-- Bool 

readBoolAP :: Reader Bool
readBoolAP = byte >>= \b ->
             assertTag b tagBool >>
             assertApplication b >>
             return (lvt b == 1)

readBoolCS :: Word8 -> Reader Bool
readBoolCS tag = byte >>= \b ->
                 assertTag b tag >>
                 assertContextSpecific b >>
                 assertLength b 1 >>
                 byte >>= \b2 ->
                 return (b2 /= 0)

-- Unsigned
{-
readUnsignedAP :: Reader Word32
readUnsignedAP = byte >>= \b ->
                 assertTag b tagUnsigned >>
                 assertApplication b >>
                 readLength b >>= \l ->
                 bytes l  >>= \byteList ->
                 return f 0 byteList
             where f a b = (a * 256) + b

-}

