{
module Dep.Vhdl.Parser(
        vhdlParser                                                              -- functions
    ) where

import Debug.Trace
import Data.Char(ord,toUpper)
import Data.List

import Dep.Structures(ANSIShow(..))
import Dep.Vhdl.Lexer(alexScanTokens,Token(..),Keyw(..),Delim(..))

}

%name vp
%tokentype {Token}
%error {parseError}
%token
    abs           {Kw Abs}
    access        {Kw Access}
    after         {Kw After}
    alias         {Kw Alias}
    all           {Kw All}
    and           {Kw And}
    architecture  {Kw Architecture}
    array         {Kw Array}
    assert        {Kw Assert}
    attribute     {Kw Attribute}
    begin         {Kw Begin}
    block         {Kw Block}
    body          {Kw Body}
    buffer        {Kw Buffer}
    bus           {Kw Bus}
    case          {Kw Case}
    component     {Kw Component}
    configuration {Kw Configuration}
    constant      {Kw Constant}
    disconnect    {Kw Disconnect}
    downto        {Kw Downto}
    else          {Kw Else}
    elsif         {Kw Elsif}
    end           {Kw End}
    entity        {Kw Entity}
    exit          {Kw Exit}
    file          {Kw File}
    for           {Kw For}
    function      {Kw Function}
    generate      {Kw Generate}
    generic       {Kw Generic}
    group         {Kw Group}
    guarded       {Kw Guarded}
    if            {Kw If}
    impure        {Kw Impure}
    in            {Kw In}
    inertial      {Kw Inertial}
    inout         {Kw Inout}
    is            {Kw Is}
    label         {Kw Label}
    library       {Kw Library}
    linkage       {Kw Linkage}
    literal       {Kw Literal}
    loop          {Kw Loop}
    map           {Kw Map}
    mod           {Kw Mod}
    nand          {Kw Nand}
    new           {Kw New}
    next          {Kw Next}
    nor           {Kw Nor}
    not           {Kw Not}
    of            {Kw Of}
    on            {Kw On}
    open          {Kw Open}
    or            {Kw Or}
    others        {Kw Others}
    out           {Kw Out}
    package       {Kw Package}
    port          {Kw Port}
    postponed     {Kw Postponed}
    procedure     {Kw Procedure}
    process       {Kw Process}
    pure          {Kw Pure}
    range         {Kw Range}
    record        {Kw Record}
    rol           {Kw Rol}
    ror           {Kw Ror}
    select        {Kw Select}
    severity      {Kw Severity}
    signal        {Kw Signal}
    shared        {Kw Shared}
    sla           {Kw Sla}
    sll           {Kw Sll}
    sra           {Kw Sra}
    srl           {Kw Srl}
    subtype       {Kw Subtype}
    then          {Kw Then}
    to            {Kw To}
    transport     {Kw Transport}
    type          {Kw Type}
    unaffected    {Kw Unaffected}
    units         {Kw Units}
    until         {Kw Until}
    use           {Kw Use}
    variable      {Kw Variable}
    wait          {Kw Wait}
    when          {Kw When}
    while         {Kw While}
    with          {Kw With}
    xnor          {Kw Xnor}
    xor           {Kw Xor}
    id            {Id $$}
    '&'           {Dl Amp}
    "'"           {Dl Acc}
    '*'           {Dl Mul}
    '+'           {Dl Add}
    ','           {Dl Comma}
    '-'           {Dl Minus}
    '.'           {Dl Dot}
    '/'           {Dl Div}
    ':'           {Dl Colon}
    ';'           {Dl Semic}
    '<'           {Dl Le}
    '='           {Dl Eq}
    '>'           {Dl Gt}
    '|'           {Dl Bar}
    '=>'          {Dl Imp}
    '**'          {Dl Pow}
    ':='          {Dl Assig}
    '/='          {Dl DivEq}
    '>='          {Dl Geq}
    '<='          {Dl Leq}
    '<>'          {Dl Neq}
    '('           {ScO False}
    ')'           {ScC False}
    '['           {ScO True}
    ']'           {ScC True}
    null          {Null}
    intli         {IntLi $$}
    decli         {DecLi $$}
    eof           {Eof}

%%

copyFoo                                                                         --TODO p
    :                                                                           { }
    ;

 -- "Hyperlinked VHDL-93 BNF Syntax": lelangagevhdl.net/doc/vhdl-93_bnf.html

abstractLiteral                                                                 --TODO p
    : decli                                                                     { EDbl $1 }
    | intli                                                                     { EInt $1 } --TODO: add based literal
    ;

accessTypeDefinition                                                            --TODO p
    : access subtypeIndication                                                  { $2 }
    ;

actualDesignator                                                                --TODO p
    : expression                                                                { }
    | name                                                                      { }  --signal, variable, or file
    | open                                                                      { }
    ;

actualParameterPart                                                             --TODO p
    : associationList                                                           { } --parameters
    ;

actualPart                                                                      --TODO p
    : actualDesignator                                                          { }
    | name '(' actualDesignator ')'                                             { }
    | actualDesignator                                                          { }
    ;

addingOperator                                                                  --TODO p
    : '+'                                                                       { $1 }
    | '-'                                                                       { $1 }
    | '&'                                                                       { $1 }
    ;

aggregate                                                                       --TODO p
    : '(' elementAssociationList ')'                                            { $2 }
    ;

aliasDeclaration                                                                --TODO p
    : alias aliasDesignator ':' subtypeIndication name signature ';'            { }
    | alias aliasDesignator ':' subtypeIndication name ';'                      { }
    | alias aliasDesignator name signature ';'                                  { }
    | alias aliasDesignator name ';'                                            { }
    ;

aliasDesignator                                                                 --TODO p
    : identifier                                                                { }
    | characterLiteral                                                          { }
    | operatorSymbol                                                            { }
    ;

allocator                                                                       --TODO p
    : new subtypeIndication                                                     { }
    | new qualifiedExpression                                                   { }
    ;

architectureBody                                                                --TODO p
    : architecture identifier of name is architectureDeclarationPart begin architectureStatementPart end architecture simplename ';'    { }
    | architecture identifier of name is architectureDeclarationPart begin architectureStatementPart end simplename ';'    { }
    | architecture identifier of name is architectureDeclarationPart begin architectureStatementPart end architecture ';'    { }
    | architecture identifier of name is architectureDeclarationPart begin architectureStatementPart end ';'    { }
    ;

architectureDeclarationPart                                                     --TODO p
    : blockDeclarativeItemListO                                                 { $1 }
    ;

architectureStatementPart                                                       --TODO p
    : concurrentStatementListO                                                  { $1 }
    ;

arrayTypeDefinition                                                             --TODO p
    : unconstrainedArrayDefinition                                              { $1 }
    | constrainedArrayDefinition                                                { $1 }
    ;

assertion                                                                       --TODO p
    : assert condition                                                          { }
    | assert condition report expression                                        { }
    | assert condition severity expression                                      { }
    | assert condition report expression severity expression                    { }
    ;

assertionStatement                                                              --TODO p
    : assertion ';'                                                             { }
    | label ':' assertion ';'                                                   { }
    ;

associationElement                                                              --TODO p
    : actualPart                                                                { }
    | formalPart '=>' actualPart                                                { }
    ;

attributeDeclaration                                                            --TODO p
    : attribute identifier ':' typeMark ';'                                     { }
    ;

attributeDesignator                                                             --TODO p
    : simplename                                                                { $1 }
    ;

attributeName                                                                   --TODO p
    : prefix signature "'" attributeDesignator '(' expression ')'               { }
    | prefix signature "'" attributeDesignator                                  { }
    | prefix "'" attributeDesignator '(' expression ')'                         { }
    | prefix "'" attributeDesignator                                            { }
    ;

attributeSpecification                                                          --TODO p
    : attribute attributeDesignator of entitySpecification is expression ';'    { }
    ;

-- TODO: based literal in grammar?

bindingIndication                                                               --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

copyFoo                                                                         --TODO p
    :                                                                           { }
    |                                                                           { }
    ;

-- lists

associationList                                                                 --TODO p
    : associationElement                                                        { $1 }
    | associationElement ',' associationList                                    { $1 : $3 }
    ;

blockDeclarativeItemListO
    :                                                                           { [] }
    | blockDeclarativeItem                                                      { [$1] }
    | blockDeclarativeItem blockDeclarativeItemListO                            { $1 : $2 }
    ;

concurrentStatementListO
    :                                                                           { [] }
    | concurrentStatement                                                       { [$1] }
    | concurrentStatement concurrentStatementListO                              { $1 : $2 }
    ;

elementAssociationList                                                          --TODO p
    : elementAssociation                                                        { $1 }
    | elementAssociation ',' elementAssociationList                             { $1 : $3 }
    ;

{

data Item = TypeDef String Domain | SubtypeDef String Domain deriving (Eq)

instance Show Item where
    show (TypeDef s d) = "type "++s++" is "++show d++";"
    show (SubtypeDef s d) = "subtype "++s++" is "++show d++";"

data Placement = PlPackage | PlPackageBody | PlBlock | PlEntity | PlArchitecture | PlConfiguration | PlProcess | PlProcedure | PlFunction deriving (Show,Enum,Bounded,Eq)

data Dir = Up | Down deriving (Enum,Eq,Bounded)

instance Show Dir where
    show Up = "to"
    show Down = "downto"

data Domain = DRang Exp Dir Exp | DEnum [String] deriving (Eq)

instance Show Domain where
    show (DRang a d b) = "range "++show a ++ ' ':show d ++ ' ':show b
    show (DEnum s) = '(':intercalate "," s++")"

data Exp = EInt Int | EDbl Double deriving (Eq)

instance Show Exp where
    show (EInt i) = show i
    show (EDbl d) = show d

parseError :: [Token] -> a
parseError x = error $ "Parser error: "++printFragment x

printFragment :: [Token] -> String
printFragment = intercalate " " . map (showAnsi)

vhdlParser :: String -> Item
vhdlParser x = trace (printFragment ax) (vp $ ax)
    where ax = alexScanTokens x

}
