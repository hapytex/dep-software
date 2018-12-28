{
module Dep.Vhdl.Lexer (
        alexScanTokens,                                                         --functions
        Token(..),Keyw(..),Delim(..)                                            -- datas
    ) where

import Data.Char (toLower,toUpper)

import Dep.Structures(ANSIShow(..))

}

%wrapper "basic"
$dec   = [0-9]                                                                  -- dec digits
$hex   = [0-9A-Fa-f]                                                            -- hex digits
$oct   = [0-7]                                                                  -- oct digits
$alf   = [a-zA-Z]                                                               -- alphabetic characters
$aln   = [a-zA-Z0-9]                                                            -- alphanumerical characters
$dsh   = \x2d                                                                   -- dash
$uds   = \x5f                                                                   -- underscore
$amp   = \x26                                                                   -- ampersand
$acc   = \x27                                                                   -- accent
$obr   = \x28                                                                   -- open bracket
$cbr   = \x29                                                                   -- close bracket
$ofb   = \x5b                                                                   -- open flat bracket
$cfb   = \x5d                                                                   -- close flat bracket
$ast   = \x2a                                                                   -- asterisk
$pls   = \x2b                                                                   -- plus
$cmm   = \x2c                                                                   -- comma
$min   = \x2d                                                                   -- minus
$dot   = \x2e                                                                   -- dot
$slh   = \x2f                                                                   -- slash
$cln   = \x3a                                                                   -- colon
$scl   = \x3b                                                                   -- semi-colon
$lea   = \x3c                                                                   -- less than
$equ   = \x3d                                                                   -- equal
$gea   = \x3e                                                                   -- greater than
$bar   = \x7c                                                                   -- vertical bar
$spc   = [\x20\n\r\t]                                                           -- whitespace
$exp   = [Ee]                                                                   -- exponent
$pmn   = [\+\-]                                                                 -- plusminus

tokens :-
    $dsh$dsh[^\r\n]*                                  ;                         -- comment
    $spc                                              ;                         -- whitespace elimination
    abs                                               {const $ Kw Abs}            -- keywords
    access                                            {const $ Kw Access}
    after                                             {const $ Kw After}
    alias                                             {const $ Kw Alias}
    all                                               {const $ Kw All}
    and                                               {const $ Kw And}
    architecture                                      {const $ Kw Architecture}
    array                                             {const $ Kw Array}
    assert                                            {const $ Kw Assert}
    attribute                                         {const $ Kw Attribute}
    begin                                             {const $ Kw Begin}
    block                                             {const $ Kw Block}
    body                                              {const $ Kw Body}
    buffer                                            {const $ Kw Buffer}
    bus                                               {const $ Kw Bus}
    case                                              {const $ Kw Case}
    component                                         {const $ Kw Component}
    configuration                                     {const $ Kw Configuration}
    constant                                          {const $ Kw Constant}
    disconnect                                        {const $ Kw Disconnect}
    downto                                            {const $ Kw Downto}
    else                                              {const $ Kw Else}
    elsif                                             {const $ Kw Elsif}
    end                                               {const $ Kw End}
    entity                                            {const $ Kw Entity}
    exit                                              {const $ Kw Exit}
    file                                              {const $ Kw File}
    for                                               {const $ Kw For}
    function                                          {const $ Kw Function}
    generate                                          {const $ Kw Generate}
    generic                                           {const $ Kw Generic}
    group                                             {const $ Kw Group}
    guarded                                           {const $ Kw Guarded}
    if                                                {const $ Kw If}
    impure                                            {const $ Kw Impure}
    in                                                {const $ Kw In}
    inertial                                          {const $ Kw Inertial}
    inout                                             {const $ Kw Inout}
    is                                                {const $ Kw Is}
    label                                             {const $ Kw Label}
    library                                           {const $ Kw Library}
    linkage                                           {const $ Kw Linkage}
    literal                                           {const $ Kw Literal}
    loop                                              {const $ Kw Loop}
    map                                               {const $ Kw Map}
    mod                                               {const $ Kw Mod}
    nand                                              {const $ Kw Nand}
    new                                               {const $ Kw New}
    next                                              {const $ Kw Next}
    nor                                               {const $ Kw Nor}
    not                                               {const $ Kw Not}
    of                                                {const $ Kw Of}
    on                                                {const $ Kw On}
    open                                              {const $ Kw Open}
    or                                                {const $ Kw Or}
    others                                            {const $ Kw Others}
    out                                               {const $ Kw Out}
    package                                           {const $ Kw Package}
    port                                              {const $ Kw Port}
    postponed                                         {const $ Kw Postponed}
    procedure                                         {const $ Kw Procedure}
    process                                           {const $ Kw Process}
    pure                                              {const $ Kw Pure}
    range                                             {const $ Kw Range}
    record                                            {const $ Kw Record}
    rol                                               {const $ Kw Rol}
    ror                                               {const $ Kw Ror}
    select                                            {const $ Kw Select}
    severity                                          {const $ Kw Severity}
    signal                                            {const $ Kw Signal}
    shared                                            {const $ Kw Shared}
    sla                                               {const $ Kw Sla}
    sll                                               {const $ Kw Sll}
    sra                                               {const $ Kw Sra}
    srl                                               {const $ Kw Srl}
    subtype                                           {const $ Kw Subtype}
    then                                              {const $ Kw Then}
    to                                                {const $ Kw To}
    transport                                         {const $ Kw Transport}
    type                                              {const $ Kw Type}
    unaffected                                        {const $ Kw Unaffected}
    units                                             {const $ Kw Units}
    until                                             {const $ Kw Until}
    use                                               {const $ Kw Use}
    variable                                          {const $ Kw Variable}
    wait                                              {const $ Kw Wait}
    when                                              {const $ Kw When}
    while                                             {const $ Kw While}
    with                                              {const $ Kw With}
    xnor                                              {const $ Kw Xnor}
    xor                                               {const $ Kw Xor}
    null                                              {const Null}              -- literals
    $dec($uds?$dec)*                                  {IntLi . read . filter ((/=) '_')}
    $dec($uds?$dec)*($dot$dec($uds?$dec)*)?$exp$pmn$dec($uds?$dec)* {DecLi . read . filter ((/=) '_')}
	$equ$gea                                          {const $ Dl Imp}            -- delimiters
	$ast$ast                                          {const $ Dl Pow}
	$cln$equ                                          {const $ Dl Assig}
	$slh$equ                                          {const $ Dl DivEq}
	$gea$equ                                          {const $ Dl Geq}
	$lea$equ                                          {const $ Dl Leq}
	$lea$equ                                          {const $ Dl Neq}
        $amp                                              {const $ Dl Amp}
	$acc                                              {const $ Dl Acc}
	$ast                                              {const $ Dl Mul}
	$pls                                              {const $ Dl Add}
	$cmm                                              {const $ Dl Comma}
	$min                                              {const $ Dl Minus}
	$dot                                              {const $ Dl Dot}
	$slh                                              {const $ Dl Div}
	$cln                                              {const $ Dl Colon}
	$scl                                              {const $ Dl Semic}
	$lea                                              {const $ Dl Le}
	$equ                                              {const $ Dl Eq}
	$gea                                              {const $ Dl Gt}
	$bar                                              {const $ Dl Bar}
	$cmm                                              {const $ Dl Comma}
	$obr                                              {const $ ScO False}         -- brackets
	$cbr                                              {const $ ScC False}
	$ofb                                              {const $ ScO True}
	$cfb                                              {const $ ScC True}
    $alf($uds?$aln)*$uds?                             {Id . map toLower}        -- basic identifier
    eof                                               {const Eof}               -- end of file token
    .                                                 {\s->error ("Unrecognized token \""++s++"\"!")} -- all the rest are exceptions

{

data Token = Id String | Kw Keyw | Dl Delim | ScO Bool | ScC Bool | Null | IntLi Int | DecLi Double | Eof

instance Show Token where
    show (Id s) = s
    show (Kw s) = show s
    show (Dl d) = show d
    show (IntLi i) = show i
    show (DecLi i) = show i
    show (ScO False) = "("
    show (ScO True) = "["
    show (ScC False) = ")"
    show (ScC True) = "]"
    show _ = ""

instance ANSIShow Token where
    showAnsi x = (premarkup $ tokType x)++show x++"\x1b[0m"

data Keyw = Abs | Access | After | Alias | All | And | Architecture | Array | Assert | Attribute |
    Begin | Block | Body | Buffer | Bus |
    Case | Component | Configuration | Constant |
    Disconnect | Downto |
    Else | Elsif | End | Entity | Exit |
    File | For | Function |
    Generate | Generic | Group | Guarded |
    If | Impure | In | Inertial | Inout | Is |
    Label | Library | Linkage | Literal | Loop |
    Map | Mod | Nand | New | Next | Nor | Not |
    Of | On | Open | Or | Others | Out |
    Package | Port | Postponed | Procedure | Process | Pure |
    Range | Record | Rol | Ror |
    Select | Severity | Signal | Shared | Sla | Sll | Sra | Srl | Subtype |
    Then | To | Transport | Type |
    Unaffected | Units | Until | Use |
    Variable |
    Wait | When | While | With | Xnor | Xor
    deriving (Eq,Enum,Bounded)

instance Show Keyw where
    show Abs = "abs"
    show Access = "access"
    show After = "after"
    show Alias = "alias"
    show All = "all"
    show And = "and"
    show Architecture = "architecture"
    show Array = "array"
    show Assert = "assert"
    show Attribute = "attribute"
    show Begin = "begin"
    show Block = "block"
    show Body = "body"
    show Buffer = "buffer"
    show Bus = "bus"
    show Case = "case"
    show Component = "component"
    show Configuration = "configuration"
    show Constant = "constant"
    show Disconnect = "disconnect"
    show Downto = "downto"
    show Else = "else"
    show Elsif = "elsif"
    show End = "end"
    show Entity = "entity"
    show Exit = "exit"
    show File = "file"
    show For = "for"
    show Function = "function"
    show Generate = "generate"
    show Generic = "generic"
    show Group = "group"
    show Guarded = "guarded"
    show If = "if"
    show Impure = "impure"
    show In = "in"
    show Inertial = "inertial"
    show Inout = "inout"
    show Is = "is"
    show Label = "label"
    show Library = "library"
    show Linkage = "linkage"
    show Literal = "literal"
    show Loop = "loop"
    show Map = "map"
    show Mod = "mod"
    show Nand = "nand"
    show New = "new"
    show Next = "next"
    show Nor = "nor"
    show Not = "not"
    show Of = "of"
    show On = "on"
    show Open = "open"
    show Or = "or"
    show Others = "others"
    show Out = "out"
    show Package = "package"
    show Port = "port"
    show Postponed = "postponed"
    show Procedure = "procedure"
    show Process = "process"
    show Pure = "pure"
    show Range = "range"
    show Record = "record"
    show Rol = "rol"
    show Ror = "ror"
    show Select = "select"
    show Severity = "severity"
    show Signal = "signal"
    show Shared = "shared"
    show Sla = "sla"
    show Sll = "sll"
    show Sra = "sra"
    show Srl = "srl"
    show Subtype = "subtype"
    show Then = "then"
    show To = "to"
    show Transport = "transport"
    show Type = "type"
    show Unaffected = "unaffected"
    show Units = "units"
    show Until = "until"
    show Use = "use"
    show Variable = "variable"
    show Wait = "wait"
    show When = "when"
    show While = "while"
    show With = "with"
    show Xnor = "xnor"
    show Xor = "xor"

data Delim = Amp | Acc | Mul | Add | Comma | Minus | Dot | Div | Colon | Semic
    | Le | Eq | Gt | Bar | Imp | Pow | Assig | DivEq | Geq | Leq | Neq
    deriving (Eq,Enum,Bounded)

instance Show Delim where
    show Amp = "&"
    show Acc = "'"
    show Mul = "*"
    show Add = "+"
    show Comma = ","
    show Minus = "-"
    show Dot = "."
    show Div = "/"
    show Colon = ":"
    show Semic = ";"
    show Le = "<"
    show Eq = "="
    show Gt = ">"
    show Bar = "|"
    show Imp = "=>"
    show Pow = "**"
    show Assig = ":="
    show DivEq = "/="
    show Geq = ">="
    show Leq = "<="
    show Neq = "<>"

data TokenType = Delimiter | Identifier | Lite | Keyword | Other deriving (Show,Eq,Enum,Bounded)

tokType :: Token -> TokenType
tokType (Id _) = Identifier
tokType (Kw _) = Keyword
tokType (Dl _) = Delimiter
tokType Null = Lite
tokType (IntLi _) = Lite
tokType (DecLi _) = Lite
tokType _ = Other

premarkup :: TokenType -> String
premarkup Keyword = "\x1b[34;1m"
premarkup Delimiter = "\x1b[35m"
premarkup Lite = "\x1b[33m"
premarkup Identifier = "\x1b[31m"
--premarkup Bracket = "\x1b[32m"
premarkup _ = ""

readBase :: Int -> String -> Int
readBase b = foldl (readBaseChar b) 0 . map ((+) (-48) . ord.toUpper)

readBaseChar :: Int -> Int -> Int -> Int
readBaseChar b x y | y < 0 = error "Character out of range."
                   | y < 10 && y < b = bx y
                   | y2 > 9 && y2 < b = bx y2
                   | otherwise = error "Character out of range."
                   where bx = (+) (b*x)
                         y2 = y-7

}
