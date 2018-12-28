{

{-# LANGUAGE TemplateHaskell #-}

module Dep.Cisc.Parser(
        ciscParser                                                              -- functions
    ) where

import Control.Monad.State.Lazy(State, state, execState, evalState)

import Debug.Trace

import Data.Char(ord,toLower)
import qualified Data.HashMap.Strict as Hm
import Data.List
import Data.Word(Word8,Word16)

import Dep.Algorithms.Cpu(Instr(..),asl,asr,Cmnd(..),cmndS,cmndL,Addr(..),CiscProgram(..),MemVar(..))
import Dep.Cisc.Lexer(alexScanTokens,Token(..))
import Dep.Lexer()
import Dep.Structures(ANSIShow(..))
import Dep.Utils(selN)
}

%name cp
%tokentype {Token}
%error {parseError}

%monad {PrsSt}

%token

    ord           {Org False}
    orp           {Org True}

    asr           {Kw $$@(Asr _)}
    asl           {Kw $$@(Asl _)}

    add           {Kw $$@Add}
    sub           {Kw $$@Sub}

    inc           {Kw $$@Inc}
    dec           {Kw $$@Dec}

    mul           {Kw $$@Mul}
    div           {Kw $$@Div}

    root          {Kw $$@Root}
    neg           {Kw $$@Neg}

    and           {Kw $$@And}
    nand          {Kw $$@Nand}
    or            {Kw $$@Or}
    nor           {Kw $$@Nor}
    xor           {Kw $$@Xor}
    xnor          {Kw $$@Xnor}
    mask          {Kw $$@Mask}
    inv           {Kw $$@Inv}

    load          {Kw $$@(Load _)}
    copy          {Kw $$@Copy}
    stor          {Kw $$@(Stor _)}

    jmp           {Kw $$@Jmp}
    cjmp          {Kw $$@Cjmp}
    jsr           {Kw $$@Jsr}
    rts           {Kw $$@Rts}

    nop           {Kw $$@Nop}

    clr           {Kw $$@Clr}
    clrs          {Kw $$@ClrS}
    sets          {Kw $$@SetS}

    gt            {Kw $$@Gt}
    ge            {Kw $$@Ge}
    lt            {Kw $$@Lt}
    le            {Kw $$@Le}
    eq            {Kw $$@Eq}
    ne            {Kw $$@Ne}

    '#'           {Hsh}
    '('           {OBr}
    ')'           {CBr}
    '+'           {Up}
    ':'           {Colon}

    lbl           {Ident $$}

    word          {Word}
    array         {Array $$}

    reg           {Reg $$}
    val           {IntLi $$}

    eof           {Eof}

%%

main
  : ord ldats orp lcmds eof  {% return (CiscProgram $2 $4)}
  ;

ldats
  :                    {[]}
  | ldat ldats         {$1:$2}
  ;

ldat
  : lbl word           {% injectInc (MemWord $1)    $1  1}
  | lbl array          {% injectInc (MemArry $1 $2) $1 $2}
  ;

lcmds
  :                    {[]}
  | lcmd lcmds         {$1:$2}
  ;

lcmd
  : cmd                {% updateHash $1}
  | lbl ':' cmd        {% injectHash $1 >> updateHash $3}
  ;

cmd
  : shftf reg reg '#' addr  {cmndS ($1 (fromIntegral $5)) $2 $3 0}
  | trisd reg reg reg       {cmndS $1 $2 $3 $4}
  | twosd reg reg           {cmndS $1 $2 $3  0}
  | twoss reg reg           {cmndS $1 0  $2 $3}
  | ones  reg               {cmndS $1 $2  0  0}
  | zers                    {cmndS $1  0  0  0}
  | jmps addr               {cmndL $1 0 0 0 $2}
  | loads                   {$1}
  | stors                   {$1}
  | srts                    {$1}
  ;

loads
  : load reg '#' addr              {cmndL (Load Imm)    $2  0  0 $4}
  | load reg addr                  {cmndL (Load Drct)   $2  0  0 $3}
  | load reg '(' addr ')'          {cmndL (Load Indrct) $2  0  0 $4}
  | load reg addr '+' '(' reg ')'  {cmndL (Load RgIdx)  $2 $6  0 $3}
  | load reg '(' reg ')'           {cmndS (Load RgInd)  $2 $4  0}
  | load reg '(' reg ')' '+' reg   {cmndS (Load RgRel)  $2 $4 $7}
  ;

stors
  : stor addr reg                  {cmndL (Stor Drct)   0 $3  0 $2}
  | stor '(' addr ')' '+' reg      {cmndL (Stor Indrct) 0 $6  0 $3}
  | stor addr '+' '(' reg ')' reg  {cmndL (Stor RgIdx) $5 $7  0 $2}
  | stor '(' reg ')' reg           {cmndS (Stor RgInd) $3 $5  0}
  | stor '(' reg ')' '+' reg reg   {cmndS (Stor RgRel) $3 $7 $6}
  ;

srts
  : jsr addr '(' reg ')'           {cmndL $1 0 $4 0 $2}
  | rts '(' reg ')'                {cmndS $1 0 $3 0}
  ;

shftf
  : asr                {asr}
  | asl                {asl}
  ;

trisd -- Instructions with three registers
  : add                {$1}
  | sub                {$1}
  | mul                {$1}
  | div                {$1}
  | and                {$1}
  | nand               {$1}
  | or                 {$1}
  | nor                {$1}
  | xor                {$1}
  | xnor               {$1}
  ;

twosd -- Instructions with two registers
  : inc                {$1}
  | dec                {$1}
  | root               {$1}
  | neg                {$1}
  | mask               {$1}
  | inv                {$1}
  | copy               {$1}
  ;

twoss
  : gt                 {$1}
  | ge                 {$1}
  | lt                 {$1}
  | le                 {$1}
  | eq                 {$1}
  | ne                 {$1}
  ;

ones
  : clr                {$1}
  ;

zers
  : nop                {$1}
  | clrs               {$1}
  | sets               {$1}
  ;

jmps
  : jmp                {$1}
  | cjmp               {$1}
  ;

addr
  : val                {$1}
  | lbl                {% lookupHash $1}
  ;

{
type Prs = (Word16,Hm.HashMap String Word16,String -> Word16)
type PrsSt = State Prs

injectInc :: a -> String -> Word16 -> PrsSt a
injectInc ret lbl dn = injectHash lbl >> incPos ret dn

injectHash :: String -> PrsSt ()
injectHash lbl = state f
    where f (n,x,y) = ((),(n,Hm.insertWith (const id) lbl n x,y))

updateHash :: Cmnd -> PrsSt Cmnd
updateHash c@CmndS{} = incPos c 1
updateHash c@CmndL{} = incPos c 2

lookupHash :: String -> PrsSt Word16
lookupHash lbl = state f
     where f s@(_,_,y) = (y lbl,s)

incPos :: a -> Word16 -> PrsSt a
incPos c dn = state f
    where f (n,x,y) = (c,(n+dn,x,y))

parseError :: [Token] -> a
parseError x = error $ "Parser error: "++printFragment x

printFragment :: [Token] -> String
printFragment = intercalate " " . map (showAnsi)

-- | Parse the given string containing a CISC program into a `CiscProgram`.
ciscParser :: String -- ^ The given string that contains a textual description of the CISC program.
    -> CiscProgram -- ^ The returning CISC program that is equivalent with the given string.
ciscParser x = cxi
    where ax = (alexScanTokens $ map toLower x)++[Eof]
          cx = cp $ ax
          scx = execState cx (0,Hm.empty,const 0)
          lta = $(selN 3 1) scx
          cxi = evalState cx (0,lta,(Hm.!) lta)

instance Read CiscProgram where
    readsPrec 0 x = [(ciscParser x,"")]
}
