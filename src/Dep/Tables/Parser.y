{

{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Dep.Tables.Parser(
        combTableParser,fSMParser,mooreFSMParser,mealyFSMParser                                 -- functions
    ) where

import Data.Char(ord,toLower)
import qualified Data.HashMap.Strict as Hm
import Data.List
import Data.Word(Word8,Word16)

import Dep.Algorithms()
import Dep.Tables.Lexer(alexScanTokens,Token(..))
import Dep.Structures(ANSIShow(..),Specifiable(specify),CombTable(CT),BitTh(D),BitThSeq,FSMState(Label,DontCare),FSM(Moore,Mealy),Three(ThLeaf))
}

%name tables
%tokentype {Token}
%error {parseError}

%token

    cbt           {PrsTy 0}
    mrm           {PrsTy 1}
    mym           {PrsTy 2}
    mmm           {PrsTy 3}

    bitseq        {BitSeq $$}
    lbl           {Labl $$}

    slh           {Slash}

    row           {Row}
    col           {Column}

    eof           {Eof}

%%

combtable
  : cbt rows lines2bs rows eof  {Cbt $ CT (cnv fst $3) (foldl (\t (y,z)->specify y z t) (ThLeaf (replicate (cnv snd $3) D)) $3)}
  | mrm rows mrhead mrtail rows eof {Fsm $ Moore (mooreStates $4) $3 (mooreTrans $3 $4) (mooreEmit $4)}
  | mmm rows mrhead mrtail rows eof {Fsm $ Moore (mooreStates $4) $3 (mooreTrans $3 $4) (mooreEmit $4)}
  | mym rows myhead mytail rows eof {Fsm $ Mealy (mealyStates $4) $3 (mealyTrans $3 $4) (mealyEmit $3 $4)}
  | mmm rows myhead mytail rows eof {Fsm $ Mealy (mealyStates $4) $3 (mealyTrans $3 $4) (mealyEmit $3 $4)}
  ;

rows
  :                            {}
  | row                        {}
  ;

lines2bs
  :                            {[]}
  | line2bs                    {[$1]}
  | line2bs row lines2bs       {$1:$3}
  ;

mrhead
  : col                        {[]}
  | col bitseq mrhead          {$2:$3}
  ;

myhead
  :                            {[]}
  | col bitseq myhead          {$2:$3}
  ;

mrtail
  :                            {[]}
  | row                        {[]}
  | row mrtaillin mrtail       {$2:$3}
  ;

mytail
  :                            {[]}
  | row                        {[]}
  | row lbl mytailin mytail    {($2,$3):$4}
  ;

mytailin
  :                                     {[]}
  | col fsmstate slh bitseq mytailin    {($2,$4):$5}
  ;

mrtaillin
  : lbl mrtailin               {slide $1 $2}
  ;

mrtailin
  : col bitseq                 {([],$2)}
  | col fsmstate mrtailin      {topup $3 $2}
  ;

line2bs
  : bitseq col bitseq          {($1,$3)}
  ;

fsmstate
  : lbl                        { Label $1 }
  | bitseq                     { bits2state $1 }
  ;

{
slide :: a -> (b,c) -> (a,b,c)
slide x (y,z) = (x,y,z)

topup :: ([a],b) -> a -> ([a],b)
topup (xs,y) x = ((x:xs),y)

bits2state :: BitThSeq -> FSMState String
bits2state [D] = DontCare
bits2state d   = error $ "Invalid finite state machine state: \""++show d++"\". No bitstrings except the don't care are allowed."

mooreStates :: [(a,b,c)] -> [a]
mooreStates = map (\(x,_,_) -> x)

mealyStates :: [(a,b)] -> [a]
mealyStates = map fst

mooreTrans :: [BitThSeq] -> [(String,[FSMState String],a)] -> String -> BitThSeq -> FSMState String
mooreTrans ins xs = curry $ (Hm.!) $ Hm.fromList $ concatMap (\(s0,s1s,_) -> zipWith (\x f->((s0,x),f)) ins s1s) xs

mealyTrans :: [BitThSeq] -> [(String,[(FSMState String,BitThSeq)])] -> String -> BitThSeq -> FSMState String
mealyTrans ins xs = curry $ (Hm.!) $ Hm.fromList $ concatMap (\(s0,s1s) -> zipWith (\x (f,_)->((s0,x),f)) ins s1s) xs

mooreEmit :: [(String,a,BitThSeq)] -> String -> BitThSeq
mooreEmit = (Hm.!) . Hm.fromList . map (\(x,_,y) -> (x,y))

mealyEmit :: [BitThSeq] -> [(String,[(FSMState String,BitThSeq)])] -> String -> BitThSeq -> BitThSeq
mealyEmit ins xs = curry $ (Hm.!) $ Hm.fromList $ concatMap (\(s0,s1s) -> zipWith (\x (_,o)->((s0,x),o)) ins s1s) xs

data ReturnTy = Cbt {cbt :: CombTable}
              | Fsm {fsm :: FSM String BitThSeq BitThSeq}


parseError :: [Token] -> a
parseError x = error $ "Parser error: "++printFragment x

printFragment :: [Token] -> String
printFragment = intercalate " " . map (showAnsi)


cnv :: (a -> [b]) -> [a] -> Int
cnv f = maximum . map (length.f)

combTableParser :: String -> CombTable
combTableParser x = cbt $ tables $ PrsTy 0:(alexScanTokens x)++[Eof]

instance Read CombTable where
    readsPrec 0 x = [(combTableParser x,"")]

mooreFSMParser :: String -> FSM String BitThSeq BitThSeq
mooreFSMParser x = fsm $ tables $ PrsTy 1:(alexScanTokens x)++[Eof]

mealyFSMParser :: String -> FSM String BitThSeq BitThSeq
mealyFSMParser x = fsm $ tables $ PrsTy 2:(alexScanTokens x)++[Eof]

fSMParser :: String -> FSM String BitThSeq BitThSeq
fSMParser x = fsm $ tables $ PrsTy 3:(alexScanTokens x)++[Eof]

instance Read (FSM String BitThSeq BitThSeq) where
    readsPrec 0 x = [(fSMParser x,"")]
}
