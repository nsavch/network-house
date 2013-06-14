module Net.Packet (InPacket,len,emptyInPack,toInPack,takeInPack,dropInPack,
		   byteAt,wordAt,toChunk,
		   OutPacket,outLen,chunks,Chunk,
		   emptyOutPack,addChunk,appendOutPack,
		   splitOutPack,outBytes,loopback,loopbackout
		  ) where

import Net.Bits
import qualified Data.ByteString as B
import Net.Utils

-- | The buffers used to represent packet,
-- when they are received over the network.
newtype InPacket = InPack { buffer  :: Chunk }

emptyInPack = InPack{buffer=B.empty}

takeInPack n (InPack buf) = InPack (B.take n buf)

dropInPack n (InPack buf) = InPack (B.drop n buf)

instance Show InPacket where
  show p = "<<"++show (B.length $ buffer p)++" bytes>>"

-- | Get a byte at a certain offset.
byteAt             :: InPacket -> Int -> Word8
p `byteAt` x        = (buffer p) `B.index` x

-- | Get a word from a certain offset (big endian).
wordAt             :: InPacket -> Int -> Word16
p `wordAt` x        = (buf `B.index` x) `nextTo` (buf `B.index` x+1)
  where buf         = buffer p

toChunk   :: InPacket -> Chunk
toChunk InPack { buffer=buf } = buf

toInPack :: Chunk -> InPacket
toInPack c = InPack {buffer=c}

len :: InPacket -> Int
len = B.length . buffer


-- | The buffers for packets, that are to be sent over the network.
-- Each array contains a header of a layer in the network protocol stack.
data OutPacket = OutPack
               { outLen  :: !Int
               , chunks  :: ![Chunk]
               }
	       -- ^Invariant: outLen==sum (map arraySize chunks)

instance Show OutPacket where show p = "<<"++show (outLen p)++" bytes>>"

type Chunk = B.ByteString
type OutPacketS = OutPacket -> OutPacket

addChunk           :: Chunk -> OutPacketS
addChunk a p        = OutPack { outLen = B.length a + outLen p , chunks = a : chunks p }

emptyOutPack        = OutPack { outLen = 0, chunks = [] }

toOutPack :: Chunk -> OutPacket
toOutPack c = OutPack (B.length c) [c]

appendOutPack p1               (OutPack 0  _  ) = p1 -- optimize special case
appendOutPack (OutPack 0  _  ) p2               = p2 -- optimize special case
appendOutPack (OutPack n1 cs1) (OutPack n2 cs2) = OutPack (n1+n2) (cs1++cs2)

{-# NOINLINE splitOutPack #-}
splitOutPack i p@(OutPack n cs) =
    if i>=n
    then (p,emptyOutPack)
    else splitChunks i n cs

{-# NOINLINE splitChunks #-}
splitChunks 0 n cs = (emptyOutPack,OutPack n cs)
splitChunks i n [] = (emptyOutPack,emptyOutPack)
splitChunks i n (c:cs) =
    if i>=n1
    then (toOutPack c,OutPack (n-n1) cs)
    else let (c1,c2) = splitChunk i c
	 in (toOutPack c1,OutPack (n-i) (c2:cs))
  where n1 = B.length c

splitChunk i c = B.splitAt i c

loopback :: OutPacket -> InPacket
loopback p = InPack {buffer=outBytes p}

outBytes = B.concat . chunks
             
loopbackout :: InPacket -> OutPacket
loopbackout inp = OutPack {outLen=B.length $ buffer inp,chunks=[toChunk inp]}
