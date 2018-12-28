{
module Dep.Cisc.Lexer (
        alexScanTokens,                                                         --functions
        Token(..),                                                              -- datas
    ) where

import Debug.Trace

import Data.Char (isDigit)
import Data.Word(Word8,Word16)

import Dep.Algorithms.Cpu(Instr(..),asr,asl,Addr(Imm))
import Dep.Lexer(TokenType(..),showAnsiToken)
import Dep.Structures(ANSIShow(..))

import Numeric(readHex)

}

%wrapper "basic"
$alp   = [A-Za-z]                                                               -- alphabet characters
$aln   = [A-Za-z0-9]                                                            -- alphanumerical characters
$dec   = [0-9]                                                                  -- dec digits
$hsh   = \x23                                                                   -- hash character
$obr   = \x28                                                                   -- open bracket
$cbr   = \x29                                                                   -- close bracket
$spc   = [\x20\n\r\t]                                                           -- whitespace
$add   = \x2b                                                                   -- add operator (addressing)
$dsh   = \x2d                                                                   -- comment dash
$reg   = \x52                                                                   -- regi
$cln   = \x3a                                                                   -- colon

tokens :-
    $dsh$dsh[^\r\n]*                                  ;                         -- comment
    $spc+                                             ;                         -- whitespace elimination

    $cln                                              {const Colon}

    org$spc+data                                      {const $ Org False}
    org$spc+program                                   {const $ Org True}

    word                                              {const Word}
    array$spc*\x5b$spc*[0-9]+$spc*\x5d                {Array . read . filter isDigit}

    r[0-7]                                            {Reg . read . tail}
    $add                                              {const Up}
    \#                                                {const Hsh}
    [0-9]+                                            {IntLi . read}
    0x[0-9a-fA-F]+                                    {IntLi . fst . head . readHex . drop 2 } -- . foldl1 (\x y -> 16*x+y) . map hexDigit }

    asr                                               {const $ Kw $ asr 0}      -- command instructions
    asl                                               {const $ Kw $asl 0}

    add                                               {const $ Kw Add}
    sub                                               {const $ Kw Sub}

    inc                                               {const $ Kw Inc}
    dec                                               {const $ Kw Dec}

    mul                                               {const $ Kw Mul}
    div                                               {const $ Kw Div}

    root                                              {const $ Kw Root}
    neg                                               {const $ Kw Neg}

    and                                               {const $ Kw And}
    nand                                              {const $ Kw Nand}
    or                                                {const $ Kw Or}
    nor                                               {const $ Kw Nor}
    xor                                               {const $ Kw Xor}
    xnor                                              {const $ Kw Xnor}
    mask                                              {const $ Kw Mask}
    inv                                               {const $ Kw Inv}

    load                                              {const $ Kw $ Load Imm}
    copy                                              {const $ Kw Copy}
    stor                                              {const $ Kw $ Stor Imm}

    jmp                                               {const $ Kw Jmp}
    cjmp                                              {const $ Kw Cjmp}
    jsr                                               {const $ Kw Jsr}
    rts                                               {const $ Kw Jmp}

    nop                                               {const $ Kw Nop}

    clr                                               {const $ Kw Clr}
    clrs                                              {const $ Kw ClrS}

    gt                                                {const $ Kw Gt}
    ge                                                {const $ Kw Ge}
    lt                                                {const $ Kw Lt}
    le                                                {const $ Kw Le}
    eq                                                {const $ Kw Eq}
    ne                                                {const $ Kw Ne}

    sets                                              {const $ Kw SetS}

    $obr                                              {const OBr }
    $cbr                                              {const CBr }

    $alp$aln*                                         {\s -> Ident s}

    .                                                 {\s->error ("Unrecognized token \""++s++"\"!")}

{

-- | The list of different tokens in a CISC program.
data Token = Kw Instr -- ^ An instruction keyword. For example `ADD`.
    | Word            -- ^ A word, used to allocate one memory position.
    | Array Word16    -- ^ An array declaration with a maximum of 65 535 words.
    | OBr             -- ^ Open bracket.
    | CBr             -- ^ Close bracket.
    | Up              -- ^ The plus sign, used for adressing.
    | Reg Word8       -- ^ A register literal.
    | Colon           -- ^ A colon, used for labels.
    | Hsh             -- ^ The hash token, used for integer literals.
    | IntLi Word16    -- ^ An integer literal.
    | Ident String    -- ^ An identifier.
    | Org Bool        -- ^ A marker of either data or program.
    | Eof             -- ^ End of file, the last token to parse.

instance Show Token where
    show (Kw s) = show s
    show Word = "word"
    show (Array n) = "array ["++show n++"]"
    show OBr = "("
    show CBr = ")"
    show Up = "+"
    show (Reg i) = 'R':show i
    show Colon = ":"
    show (IntLi v) = show v
    show Hsh = "#"
    show (Ident v) = v
    show (Org False) = "org data"
    show (Org True) = "org program"
    show Eof = "EOF"
    -- show _ = "<unknown token>"

tokType :: Token -> TokenType
tokType (Kw _) = Keyword
tokType (Org _) = Keyword
tokType Word = Keyword
tokType (Array _) = Keyword
tokType (IntLi _) = Literal
tokType OBr = Delimiter
tokType CBr = Delimiter
tokType Colon = Delimiter
tokType (Ident _) = Identifier
tokType (Reg _) = Identifier
tokType Hsh = Literal
tokType _ = Other

instance ANSIShow Token where
    showAnsi = showAnsiToken tokType

}
