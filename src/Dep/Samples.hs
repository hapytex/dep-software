module Dep.Samples (
        fsm0,fsm0s,fsm0i,fsm0t,fsm0e,                                           -- FSM0
        fsm1,fsm1s,fsm1i,fsm1t,fsm1e,                                           -- FSM1
        fsm2,fsm2s,fsm2i,fsm2t,fsm2e,                                           -- FSM2
        defaultWavehead
    ) where

import Dep.Structures

sla :: FSMState String
sla = Label "a"
slb :: FSMState String
slb = Label "b"
slc :: FSMState String
slc = Label "c"
sld :: FSMState String
sld = Label "d"
sle :: FSMState String
sle = Label "e"
slf :: FSMState String
slf = Label "f"
slg :: FSMState String
slg = Label "g"
slh :: FSMState String
slh = Label "h"
sli :: FSMState String
sli = Label "i"
slj :: FSMState String
slj = Label "j"
slk :: FSMState String
slk = Label "k"
sll :: FSMState String
sll = Label "l"
slm :: FSMState String
slm = Label "m"
sln :: FSMState String
sln = Label "n"
slo :: FSMState String
slo = Label "o"
slx :: FSMState a
slx = DontCare

fsm0 :: FSM String BitThSeq BitThSeq
fsm0 = Moore fsm0s fsm0i fsm0t fsm0e

fsm0s :: [String]
fsm0s = ["a","b","c","d","e","f"]

fsm0i :: [BitThSeq]
fsm0i = [[F,F],[F,T],[T,T],[T,F]]

fsm0t :: String -> BitThSeq -> FSMState String
fsm0t "a" [F,F] = sla
fsm0t "a" [F,T] = slb
fsm0t "a" [T,T] = slx
fsm0t "a" [T,F] = slc
fsm0t "b" [F,F] = sla
fsm0t "b" [F,T] = slb
fsm0t "b" [T,T] = slf
fsm0t "b" [T,F] = slc
fsm0t "c" [F,F] = sla
fsm0t "c" [F,T] = slx
fsm0t "c" [T,T] = sld
fsm0t "c" [T,F] = slc
fsm0t "d" [F,F] = slx
fsm0t "d" [F,T] = slb
fsm0t "d" [T,T] = sld
fsm0t "d" [T,F] = slc
fsm0t "e" [F,F] = sla
fsm0t "e" [F,T] = sle
fsm0t "e" [T,T] = slf
fsm0t "e" [T,F] = slx
fsm0t "f" [F,F] = slx
fsm0t "f" [F,T] = sle
fsm0t "f" [T,T] = slf
fsm0t "f" [T,F] = slc
fsm0t _   _     = error "fsm function contains error."

fsm0e :: String -> BitThSeq
fsm0e "a" = [F]
fsm0e "b" = [F]
fsm0e "c" = [F]
fsm0e "d" = [F]
fsm0e "e" = [T]
fsm0e "f" = [T]
fsm0e _   = error "fsm function contains error."

fsm1 :: FSM String BitTh BitTh
fsm1 = Moore fsm1s fsm1i fsm1t fsm1e

fsm1s :: [String]
fsm1s = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o"]

fsm1i :: [BitTh]
fsm1i = [F,T]

fsm1t :: String -> BitTh -> FSMState String
fsm1t "a" F = slb
fsm1t "a" T = slc
fsm1t "b" F = sld
fsm1t "b" T = sle
fsm1t "c" F = slf
fsm1t "c" T = slg
fsm1t "d" F = slh
fsm1t "d" T = sli
fsm1t "e" F = slj
fsm1t "e" T = slk
fsm1t "f" F = sll
fsm1t "f" T = slm
fsm1t "g" F = sln
fsm1t "g" T = slo
fsm1t "h" F = slh
fsm1t "h" T = sli
fsm1t "i" F = slj
fsm1t "i" T = slk
fsm1t "j" F = sll
fsm1t "j" T = slm
fsm1t "k" F = sln
fsm1t "k" T = slo
fsm1t "l" F = slh
fsm1t "l" T = sli
fsm1t "m" F = slj
fsm1t "m" T = slk
fsm1t "n" F = sll
fsm1t "n" T = slm
fsm1t "o" F = sln
fsm1t "o" T = slo
fsm1t _   _ = error "fsm function contains error."

fsm1e :: String -> BitTh
fsm1e "a" = F
fsm1e "b" = F
fsm1e "c" = F
fsm1e "d" = F
fsm1e "e" = F
fsm1e "f" = F
fsm1e "g" = F
fsm1e "h" = F
fsm1e "i" = F
fsm1e "j" = T
fsm1e "k" = F
fsm1e "l" = F
fsm1e "m" = F
fsm1e "n" = F
fsm1e "o" = F
fsm1e _   = error "fsm function contains error"

fsm2 :: FSM String BitTh BitTh
fsm2 = Mealy fsm2s fsm2i fsm2t fsm2e

fsm2s :: [String]
fsm2s = ["a","b","c","d","e","f","g"]

fsm2i :: [BitTh]
fsm2i = [F,T]

fsm2t :: String -> BitTh -> FSMState String
fsm2t "a" F = slb
fsm2t "a" T = slc
fsm2t "b" F = sld
fsm2t "b" T = sle
fsm2t "c" F = slf
fsm2t "c" T = slg
fsm2t "d" F = sld
fsm2t "d" T = sle
fsm2t "e" F = slf
fsm2t "e" T = slg
fsm2t "f" F = sld
fsm2t "f" T = sle
fsm2t "g" F = slf
fsm2t "g" T = slg
fsm2t _   _ = error "fsm function contains error."

fsm2e :: String -> BitTh -> BitTh
fsm2e "a" F = F
fsm2e "a" T = F
fsm2e "b" F = F
fsm2e "b" T = F
fsm2e "c" F = F
fsm2e "c" T = F
fsm2e "d" F = F
fsm2e "d" T = F
fsm2e "e" F = T
fsm2e "e" T = F
fsm2e "f" F = F
fsm2e "f" T = F
fsm2e "g" F = F
fsm2e "g" T = F
fsm2e _   _ = error "fsm function contains error."

defaultWavehead :: Wavehead
defaultWavehead = Wavehead ["Clk","A"] [] --F,concat [[(2*w*t+w,T),(2*w*t+2*w,F)]|t<-[0..]]),("A",T,[(15,F),(999,T)])]
