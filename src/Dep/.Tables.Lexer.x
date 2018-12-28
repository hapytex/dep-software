{
module Dep.Tables.Lexer (
        alexScanTokens,                                                         --functions
        Token(..),                                                         --datas
    ) where

import Debug.Trace

import Data.Char (isDigit)
import Data.Word(Word8,Word16)

import Dep.Algorithms.Cpu(Instr(..),asr,asl,Addr(Imm))
import Dep.Lexer(TokenType(..),showAnsiToken)
import Dep.Structures(BitThSeq,ANSIShow(..))

import Numeric(readHex)

}

%wrapper "basic"
$nln   = [\r\n]                                                                 -- new line
$hln   = [\x2a\x2b\x7c\x2d]                                                     -- horizontal rule characters
$bith  = [01\x2d]                                                               -- binary sequence
$alp   = [A-Za-z]                                                               -- alphan characters
$pip   = \x7c                                                                   -- pipe character
$slh   = \x2f                                                                   -- slash character
$spc   = [\x20\t]                                                               -- whitespace (no new line)

tokens :-
    $spc+                                             ;                         -- whitespace elimination
    $nln+($hln+$nln+)*                                { const Row }
    $pip                                              { const Column }
    $slh                                              { const Slash }
    $bith+                                            { BitSeq . read }
    $alp+                                             { Labl }
    .                                                 { \s->error ("Unrecognized token \""++s++"\"!") }

{
data Token = PrsTy Int | BitSeq BitThSeq | Labl String | Slash | Row | Column | Eof
             deriving Show
{-
instance Show Token where
    show (BitSeq b) = show b
    show (Labl s) = s
    show Slash = "/"
    show Column = "|"
    show Row = "\n"
    show _ = ""
--}

tokType :: Token -> TokenType
tokType (BitSeq _) = Literal
tokType (Labl _) = Literal
tokType _ = Other

instance ANSIShow Token where
    showAnsi = showAnsiToken tokType
}
