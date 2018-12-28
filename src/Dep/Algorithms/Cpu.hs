{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, OverloadedStrings, TupleSections #-}

module Dep.Algorithms.Cpu (
    CiscProgram(..),Cmnd(..),MemVar(..),memSize,
    Instr(Asr,Asl,Add,Sub,Inc,Dec,Mul,Div,Root,Neg,And,Nand,Or,Nor,Xor,Xnor,Mask,Inv,Load,Copy,Stor,Jmp,Cjmp,Jsr,Rts,
         Nop,Clr,ClrS,Gt,Ge,Lt,Le,Eq,Ne,SetS),
    asr,asl,
    showInstr,showCmnd,
    cmndS,cmndL,Addr(..),
    CiscBin(..)
) where

import Data.Aeson(Value(String, Null), ToJSON(toJSON), FromJSON(parseJSON), object, (.=))
import Data.Array.IArray()
import Data.Array.Unboxed
import Data.Bits (Bits(..),testBit,shiftR,(.&.))
import Data.Char(toUpper)
import Data.Foldable(Foldable(),toList)
import Data.List(intercalate)
import Data.Text(pack)
import Data.Word (Word8(..),Word16(..))

import Dep.Structures(BitTh(..),Serializeable(..))
import Dep.Utils(varRg,(.*),showBinary)

default(String)

toreg :: Word8 -> Word8
toreg = (7 .&. )

-- | The adressing mode when loading or storing a data element from/into memory.
data Addr = Imm | -- ^ Immediate.
            Drct | -- ^ Direct.
            Indrct | -- ^ Indirect.
            RgIdx | -- ^ Register indexed.
            RgInd | -- ^ Register indirect.
            RgRel --register relative
    deriving (Show,Enum,Bounded,Eq,Ord)

instance ToJSON Addr where
    toJSON = toJSON . fromEnum

instance FromJSON Addr where
    parseJSON t = fmap toEnum (parseJSON t)

-- Determine if the given addressing type requires a long command (an extra word
-- for the address), or a short one (encoded in the command).
longAddr :: Addr -- ^ The type of addressing to check for.
         -> Bool -- ^ A boolean that is True if it requires a long command; False otherwise.
longAddr = (>) RgInd

-- | The different kind of instructions that are supported by the processor.
data Instr = Asr {shiftd :: Word8} -- ^ Arithmetic shift to the right.
           | Asl {shiftd :: Word8} -- ^ Arithmetic shift to the left.
           | Add -- ^ Adding up the two numbers.
           | Sub -- ^ Subtract the latter from the first.
           | Inc -- ^ Increment the given number.
           | Dec -- ^ Decrement the given number.
           | Mul -- ^ Multiply the two given numbers.
           | Div -- ^ Divide the latter from the first.
           | Root -- ^ Calculate the square root of the given number.
           | Neg -- ^ Calculate the negation of the given number.
           | And -- ^ Calculate the bitwise AND function on the two numbers.
           | Nand -- ^ Calculate the bitwise NAND function on the two numbers.
           | Or -- ^ Calculate the bitwise OR function on the two numbers.
           | Nor -- ^ Calculate the bitwise NOR function on the two numbers.
           | Xor -- ^ Calculate the bitwise XOR function on the two numbers.
           | Xnor -- ^ Calculate the bitwise XNOR function on the two numbers.
           | Mask -- ^  Calculate the bitwise AND function with one on the given number.
           | Inv -- ^ Calculate the bitwise NOT function on the given number.
           | Load {mode :: Addr} -- ^ Load the given address (according to the given adressing mode) into the register.
           | Copy -- ^ Copy the value stored in one register to another register.
           | Stor {mode :: Addr} -- ^ Store the value of the given register in memory.
           | Jmp -- ^ Jump unconditionally to some address in the program.
           | Cjmp -- ^ Jump conditionally to some adress in the program.
           | Jsr -- ^ Jump as a subroutine to some address in the program adding to the call stack.
           | Rts -- ^ Return from a subroutine by popping the callstack.
           | Nop -- ^ Do nothing, the "No operation".
           | Clr -- ^ Clear the given register.
           | ClrS -- ^ Clear the status flag.
           | Gt -- ^ Check if the first number is greater than the second given number.
           | Ge -- ^ Check if the first number is greater than or equal to the second given number.
           | Lt -- ^ Check if the first number is less than the second given number.
           | Le -- ^ Check if the first number is less than or equal to the second given number.
           | Eq -- ^ Check if the two given numbers are equal.
           | Ne -- ^ Check if the two given numbers are not equal.
           | SetS -- ^ Set the status flag.
           deriving Show

instance ToJSON Instr where
    toJSON (Asr n) = object ["instr" .= "Asr", "shift" .= n]
    toJSON (Asl n) = object ["instr" .= "Asl", "shift" .= n]
    toJSON (Load m) = object [ "instr" .= "Load", "mode" .= m]
    toJSON (Stor m) = object [ "instr" .= "Stor", "mode" .= m]
    toJSON x = String (pack (show x))


longInstr  :: Instr -> Bool
longInstr Jsr       = True
longInstr Jmp       = True
longInstr Cjmp      = True
longInstr (Load x)  = longAddr x
longInstr (Stor x)  = longAddr x
longInstr _         = False

bin2instr ::  Word16 -> Instr
bin2instr   n | n < 8 = Asr $ fromIntegral n
              | n < 16 = Asr $ fromIntegral $ n-8
bin2instr  16 = Add
bin2instr  17 = Sub
bin2instr  18 = Inc
bin2instr  19 = Dec
bin2instr  20 = Mul
bin2instr  21 = Div
bin2instr  22 = Root
bin2instr  23 = Neg
bin2instr  24 = And
bin2instr  25 = Nand
bin2instr  26 = Or
bin2instr  27 = Nor
bin2instr  28 = Xor
bin2instr  29 = Xnor
bin2instr  30 = Mask
bin2instr  31 = Inv
bin2instr   n | n < 38 = Load $ toEnum $ fromIntegral $ n-32
bin2instr   n | n >= 48 && n < 54 = Stor $ toEnum $ fromIntegral $ max 1 $ n-48
bin2instr   n | n < 54 = Copy
bin2instr  64 = Jmp
bin2instr  72 = Cjmp
bin2instr  80 = Jsr
bin2instr  88 = Rts
bin2instr  96 = Nop
bin2instr 104 = Clr
bin2instr 108 = ClrS
bin2instr 112 = Gt
bin2instr 113 = Ge
bin2instr 114 = Lt
bin2instr 115 = Le
bin2instr 116 = Eq
bin2instr 117 = Ne
bin2instr 120 = SetS
bin2instr   _ = Nop

bin2cmnd :: [Word16] -> [Cmnd]
bin2cmnd (wa:wl) | not (longInstr instr) = CmndS instr d s1 s2 : bin2cmnd wl
                 | (wb:ws) <- wl = CmndL instr d s1 s2 wb : bin2cmnd ws
                 | otherwise = []
    where instr = bin2instr $ shiftR wa 9
          d = fromIntegral $ shiftR wa 6 .&. 7
          s1 = fromIntegral $ shiftR wa 3 .&. 7
          s2 = fromIntegral $ wa .&. 7
bin2cmnd [] = []

data MemVar = MemWord { memName :: String } |
              MemArry { memName :: String, _memSize :: Word16 } deriving Show

instance Serializeable MemVar where
    serialize   MemWord{} = replicate 16 D
    serialize m@MemArry{} = replicate (16*fromIntegral (_memSize m)) D

memSize :: MemVar -> Word16
memSize   MemWord{} = 1
memSize m@MemArry{} = _memSize m

data Cmnd = CmndS {instruction :: Instr, dest :: Word8, s1 :: Word8, s2 :: Word8} |
            CmndL {instruction :: Instr, dest :: Word8, s1 :: Word8, s2 :: Word8, addr :: Word16}
            deriving Show

data CiscProgram = CiscProgram {ciscVars :: [MemVar], ciscCmnds :: [Cmnd]} deriving Show

newtype CiscBin = CB [Word16]

instance Show CiscBin where
    show (CB d) = shwbin d $ bin2cmnd d

shwbin :: [Word16] -> [Cmnd] -> String
shwbin (x0:x1:xs) (c@CmndL{}:cs) = showBinary x0 ++ "  " ++ showCmndFull c ++ "\n" ++ showBinary x1 ++ "\n" ++ shwbin xs cs
shwbin (x0:xs) (c@CmndS{}:cs) = showBinary x0 ++ "  " ++ showCmndFull c ++ "\n" ++ shwbin xs cs
shwbin _ _ = []

showInstr :: Instr -> String
showInstr Asr{} = "ASR"
showInstr Asl{} = "ASL"
showInstr Load{} = "LOAD"
showInstr Stor{} = "STOR"
showInstr x =  map toUpper $ show x

showVal :: Show a => a -> String
showVal x = "\x1b[34m#" ++ show x ++ "\x1b[0m"

showAddr :: Show a => a -> String
showAddr x = "\x1b[35m" ++ show x ++ "\x1b[0m"

showReg :: Show a => a -> String
showReg x = "\x1b[31mR"++show x ++ "\x1b[0m"

showArgs :: (Num a, Eq a) => [a] -> Cmnd -> [String]
showArgs ls c = sa ls
    where sa (x:xs) | x == 0 = tl dest
                    | x == 1 = tl s1
                    | otherwise = tl s2
              where tl f = showReg (f c) : sa xs
          sa [] = []

instRegs :: Instr -> [Int]
instRegs Clr{}  = [0]       --ones
instRegs Load{} = [0]
instRegs Asr{}  = [0,1]     --shftf
instRegs Asl{}  = [0,1]
instRegs Inc{}  = [0,1]     --twosd
instRegs Dec{}  = [0,1]
instRegs Root{} = [0,1]
instRegs Neg{}  = [0,1]
instRegs Mask{} = [0,1]
instRegs Inv{}  = [0,1]
instRegs Copy{} = [0,1]
instRegs Add{}  = [0,1,2]   --trisd
instRegs Sub{}  = [0,1,2]
instRegs Mul{}  = [0,1,2]
instRegs Div{}  = [0,1,2]
instRegs And{}  = [0,1,2]
instRegs Nand{} = [0,1,2]
instRegs Or{}   = [0,1,2]
instRegs Nor{}  = [0,1,2]
instRegs Xor{}  = [0,1,2]
instRegs Xnor{} = [0,1,2]
instRegs Gt{}   = [1,2]     --twoss
instRegs Ge{}   = [1,2]
instRegs Lt{}   = [1,2]
instRegs Le{}   = [1,2]
instRegs Eq{}   = [1,2]
instRegs Ne{}   = [1,2]
instRegs _      = []

instrTl :: Cmnd -> [String]
instrTl c | Asr w <- ins = [showVal w]
          | Asl w <- ins = [showVal w]
          | Load ad <- ins = showLoad ad
          | Stor ad <- ins = showStor ad
          | Jsr <- ins = [sadr,'(':ss1++")"]
          | Rts <- ins = ['(':ss1++")"]
    where ins = instruction c
          adr = addr c
          sadr = showAddr adr
          ss1 = showReg (s1 c)
          ss2 = showReg (s2 c)
          sd = showReg (dest c)
          showLoad Imm = [showVal adr]
          showLoad Drct = [sadr]
          showLoad Indrct = ["("++showAddr adr++")"]
          showLoad RgIdx = [sadr++"+("++ss1++")"]
          showLoad RgInd = ["("++ss1++")"]
          showLoad RgRel = ["("++ss1++")+"++ss2]
          showStor Drct = [sadr,ss1]
          showStor Indrct = ["("++showAddr adr++")",ss1]
          showStor RgIdx = [sadr++"+("++sd++")",ss1]
          showStor RgInd = ["("++sd++")",ss1]
          showStor RgRel = ["("++sd++")+"++ss2,ss1]
          showStor _ = []
instrTl (CmndL _ _ _ _ v) = [showVal v]
instrTl _ = []

showCmnd :: Cmnd -> [String]
showCmnd c = ("\x1b[32m"++showInstr (instruction c) ++ "\x1b[0m") : showArgs (instRegs $ instruction c) c ++ instrTl c

showCmndFull x = unwords (showCmnd x) ++ "\x1b[0m"


asr :: Word8 -> Instr
asr = Asr . toreg

asl :: Word8 -> Instr
asl = Asl . toreg

cmndS :: Instr -> Word8 -> Word8 -> Word8 -> Cmnd
cmndS ins d s1 = CmndS ins (toreg d) (toreg s1) . toreg

cmndL :: Instr -> Word8 -> Word8 -> Word8 -> Word16 -> Cmnd
cmndL ins d s1 s2 = CmndL ins (toreg d) (toreg s1) (toreg s2)

{- Compile instructions to bits -}
insType :: Instr -> [BitTh]
insType (Asr _)  = [F,F] --00
insType (Asl _)  = [F,F]
insType Add      = [F,F]
insType Sub      = [F,F]
insType Inc      = [F,F]
insType Dec      = [F,F]
insType Mul      = [F,F]
insType Div      = [F,F]
insType Root     = [F,F]
insType Neg      = [F,F]
insType And      = [F,F]
insType Nand     = [F,F]
insType Or       = [F,F]
insType Nor      = [F,F]
insType Xor      = [F,F]
insType Xnor     = [F,F]
insType Mask     = [F,F]
insType Inv      = [F,F]
insType (Load _) = [F,T] --32
insType Copy     = [F,T]
insType (Stor _) = [F,T]
insType Jmp      = [T,F] --64
insType Cjmp     = [T,F]
insType Jsr      = [T,F]
insType Rts      = [T,F]
insType _        = [T,T] --96

insOperation :: Instr -> [BitTh]
insOperation (Asr x)  = F : F : biths 3 x
insOperation (Asl x)  = F : T : biths 3 x
insOperation Add  = [T,F,F,F,F]
insOperation Sub  = [T,F,F,F,T]
insOperation Inc  = [T,F,F,T,F]
insOperation Dec  = [T,F,F,T,T]
insOperation Mul  = [T,F,T,F,F]
insOperation Div  = [T,F,T,F,T]
insOperation Root = [T,F,T,T,F]
insOperation Neg  = [T,F,T,T,T]
insOperation And  = [T,T,F,F,F]
insOperation Nand = [T,T,F,F,T]
insOperation Or   = [T,T,F,T,F]
insOperation Nor  = [T,T,F,T,T]
insOperation Xor  = [T,T,T,F,F]
insOperation Xnor = [T,T,T,F,T]
insOperation Mask = [T,T,T,T,F]
insOperation Inv  = [T,T,T,T,T]
insOperation (Load x) = F : insMode x
insOperation Copy = [D,D,T,F,T]
insOperation (Stor x) = T : insMode x
insOperation Jmp  = [F,F]       -- 64
insOperation Cjmp = [F,T]       -- 72
insOperation Jsr  = [T,F]       -- 80
insOperation Rts  = [T,T]       -- 88
insOperation Nop  = [F,F]       -- 96
insOperation Clr  = [F,T,F]     --104
insOperation ClrS = [F,T,T]     --108
insOperation Gt   = [T,F,F,F,F] --112
insOperation Ge   = [T,F,F,F,T] --113
insOperation Lt   = [T,F,F,T,F] --114
insOperation Le   = [T,F,F,T,T] --115
insOperation Eq   = [T,F,T,F,F] --116
insOperation Ne   = [T,F,T,F,T] --117
insOperation SetS = [T,T]       --120

insMode :: Addr -> [BitTh]
insMode Imm    = [F,F,F,F]
insMode Drct   = [F,F,F,T]
insMode Indrct = [F,F,T,F]
insMode RgIdx  = [F,T,F,F]
insMode RgInd  = [T,F,T,F]
insMode RgRel  = [T,F,T,T]

biths :: Bits a => Int -> a -> [BitTh]
biths n v | n < 0 = []
          | testBit v n = T : tail
          | otherwise = F : tail
          where tail = biths (n-1) v

instance Serializeable Instr where
    serialize x = dh ++ replicate (7-length dh) D
        where dh = insType x ++ insOperation x

instance Serializeable Cmnd where
    serialize (CmndS i d s1 s2) = serialize i ++ biths 2 d ++ biths 2 s1 ++ biths 2 s2
    serialize (CmndL i d s1 s2 ac) = serialize i ++ biths 2 d ++ biths 2 s1 ++ biths 2 s2 ++ biths 15 ac

instance Serializeable CiscProgram where
    serialize (CiscProgram dt cmnds) = concatMap serialize dt++concatMap serialize cmnds

data CpuState = PS { registers :: UArray Word8 Word16, ir :: Word16, memory :: UArray Word16 Word16, pc :: Word16, status :: Bool } deriving Show

initMem :: (Ix i, Num i, Enum i, Num v, Bounded i, IArray UArray v) => UArray i v
initMem = array (0,mb) $ map (,0) [0..mb]
    where mb = maxBound

loadMem :: (Ix i, Num i, Enum i, Num v, Bounded i, IArray UArray v, Foldable f) => f v -> UArray i v -> UArray i v
loadMem fld = (// (zip [0..maxBound] $ toList fld))

initialState = PS {registers = array (0,7) $ map (,0) [0..7],
                   memory = initMem,
                   pc = 0,
                   ir = 0,
                   status = False}

iroot :: Integral i => i -> i
iroot 0 = 0
iroot 1 = 1
iroot n = go 0 0
    where go i j | j2 >= n = go j j2
                 | otherwise = go' j j2
              where j2 = j*j
          go' i j | j <= i = i
                  | m*m > n = go' i m
                  | otherwise = go' m j
              where m = div (i+j) 2

rfOp :: Instr -> Maybe (Word16 -> Word16 -> Word16)
rfOp (Asr x) = Just $ varRg $ flip shiftR $ fromInteger $ toInteger x
rfOp (Asl x) = Just $ varRg $ flip shiftL $ fromInteger $ toInteger x
rfOp Add  = Just (+)
rfOp Sub  = Just (-)
rfOp Inc  = Just $ varRg $ (+) 1
rfOp Dec  = Just $ varRg $ (+) maxBound
rfOp Mul  = Just (*)
rfOp Div  = Just div
rfOp Root = Just $ varRg iroot
rfOp Neg  = Just $ varRg negate
rfOp And  = Just (.&.)
rfOp Nand = Just $ complement .* (.&.)
rfOp Or   = Just (.|.)
rfOp Nor  = Just $ complement .* xor
rfOp Xor  = Just xor
rfOp Xnor = Just (\x -> complement . xor x)
rfOp Mask = Just $ varRg $ (.&.) 1
rfOp Inv  = Just $ varRg complement
rfop _ = Nothing

execute :: Cmnd -> CpuState -> CpuState
execute c | Just f <- rfOp ci = executeRfop f rd rs1 rs2
          | otherwise = id
          where ci = instruction c
                rd = dest c
                rs1 = s1 c
                rs2 = s2 c

executeRfop :: (Word16 -> Word16 -> Word16) -> Word8 -> Word8 -> Word8 -> CpuState -> CpuState
executeRfop f rd rs1 rs2 st = st { registers = rf' }
    where rf = registers st
          rf' = rf // [(rd,f (rf!rs1) (rf!rs2))]
