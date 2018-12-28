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
    kwlabel       {Kw Label}
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

-- source: "Hyperlinked VHDL-93 BNF Syntax"; lelangagevhdl.net/doc/vhdl-93_bnf.html
-- source: "CV: Grammar of the VHDL Subset"; http://www.cs.cmu.edu/~cmuvhdl/vhdl-grammar.html

-- 1. Design entities and configurations

-- 1.1 Entity declarations

entity_declaration
    : entity identifier is entity_header begin end entity identifier ';'         { }
    | entity identifier is entity_header begin end entity                        { }
    | entity identifier is entity_header begin end identifier                    { }
    | entity identifier is entity_header begin end                               { }
    | entity identifier is entity_header end entity identifier                   { }
    | entity identifier is entity_header end entity                              { }
    | entity identifier is entity_header end identifier                          { }
    | entity identifier is entity_header end                                     { }
    ;

entity_header
    :                                                                            { }
    | formal_port_clause                                                         { }
    ;

port_clause
    : port '(' port_list ')' ';'                                                 { }
    ;

port_list
    : port_interface_list                                                        { }
    ;


-- 1.2 Architecture bodies

architecture_body
    : architecture identifier of identifier is architecture_declarative_part 
      begin architecture_statement_part end architecture identifier ';'          { }
    | architecture identifier of identifier is architecture_declarative_part
      begin architecture_statement_part end architecture ';'                     { }
    | architecture identifier of identifier is architecture_declarative_part
      begin architecture_statement_part end identifier ';'                       { }
    | architecture identifier of identifier is architecture_declarative_part
      begin architecture_statement_part end ';'                                  { }
    ;

architecture_declarative_part
    :                                                                            { }
    | block_declarative_item architecture_declarative_part                       { }
    ;

architecture_statement_part
    :                                                                            { }
    | concurrent_statement architecture_statement_part                           { }
    ;


-- 2. Subprograms and Packages

-- 2.1 Package declarations

package_declaration
    : package identifier is package_declarative_part end package identifier ';'  { }
    | package identifier is package_declarative_part end package ';'             { }
    | package identifier is package_declarative_part end identifier ';'          { }
    | package identifier is package_declarative_part end ';'                     { }
    ;

package_declarative_part
    :                                                                            { }
    | package_declarative_item package_declarative_part                          { }
    ;

package_declarative_item
    : type_declaration                                                           { }
    | subtype_declaration                                                        { }
    | constant_declaration                                                       { }
    | use_clause                                                                 { }
    ;


-- 2.2 Package bodies

package_body
    : package body identifier is package_body_declarative_part end 
      package body identifier ';'                                                { }
    | package body identifier is package_body_declarative_part end
      package body ';'                                                           { }
    | package body identifier is package_body_declarative_part end
      identifier ';'                                                             { }
    | package body identifier is package_body_declarative_part end ';'           { }
    ;

package_body_declarative_part
    :                                                                            { }
    | package_body_declarative_item package_body_declarative_part                { }
    ;

package_body_declarative_item
    : type_declaration                                                           { }
    | subtype_declaration                                                        { }
    | constant_declaration                                                       { }
    | use_clause                                                                 { }
    ;


-- 3. Types

-- 3.1 Scalar types

scalar_type_definition
    : enumeration_type_definition                                                { }
    | integer_type_definition                                                    { }
    ;

enumeration_type_definition
    : '(' enumeration_literal_list ')'                                           { }
    ;

enumeration_literal_list
    : enumeration_literal                                                        { }
    | enumeration_literal ',' enumeration_literal_list                           { }
    ;

enumeration_literal
    : identifier                                                                 { }
    | character_literal                                                          { }
    ;

integer_type_definition
    : range_constraint                                                           { }
    ;

range_constraint
    : range range_nt                                                             { }
    ;

range_nt
    : simple_expression direction simple_expression                              { }
    ;

direction
    : to                                                                         { }
    | downto                                                                     { }
    ;


-- 4. Declarations

declaration
    : type_declaration                                                           { }
    | subtype_declaration                                                        { }
    | object_declaration                                                         { }
    | interface_declaration                                                      { }
    | entity_declaration                                                         { }
    | package_declaration                                                        { }
    ;

-- 4.1 Type declarations

type_declaration
    : full_type_declaration                                                      { }
    ;

full_type_declaration
    : type identifier is type_definition ';'                                     { }
    ;

type_definition
    : scalar_type_definition                                                     { }
    ;


-- 4.2 Subtype declarations

subtype_declaration
    : subtype identifier is subtype_indication ';'                               { }
    ;

subtype_indication
    : type_mark constraint                                                       { }
    | type_mark                                                                  { }
    ;

type_mark
    : identifier                                                                 { }
    ;

constraint
    : range_constraint                                                           { }
    ;


-- 4.3 Object declarations


object_declaration
    : constant_declaration                                                       { }
    | signal_declaration                                                         { }
    | variable_declaration                                                       { }
    ;

constant_declaration
    : constant identifier_list ':' subtype_indication ':=' expression ';'        { }
    ;

signal_declaration
    : signal identifier_list ':' subtype_indication ':=' expression ';'          { }
    | signal identifier_list ':' subtype_indication                              { }
    ;

variable_declaration
    : variable identifier_list ':' subtype_indication ':=' expression ';'        { }
    | variable identifier_list ':' subtype_indication                            { }
    ;

interface_declaration
    : interface_signal_declaration                                               { }
    ;

interface_signal_declaration
    : signal identifier_list ':' mode subtype_indication ':=' static_expression  { }
    | signal identifier_list ':' mode subtype_indication                         { }
    | signal identifier_list ':' subtype_indication ':=' static_expression       { }
    | signal identifier_list ':' subtype_indication                              { }
    | identifier_list ':' mode subtype_indication ':=' static_expression         { }
    | identifier_list ':' mode subtype_indication                                { }
    | identifier_list ':' subtype_indication ':=' static_expression              { }
    | identifier_list ':' subtype_indication                                     { }
    ;

mode
    : in                                                                         { }
    | out                                                                        { }
    ;

interface_list
    : interface_element                                                          { }
    | interface_element ';' interface_list                                       { }
    ;

interface_element
    : interface_declaration                                                      { }
    ;


-- 6. Names

name
    : simple_name                                                                { }
    | operator_symbol                                                            { }
    | selected_name                                                              { }
    ;

-- 6.1 Simple names

simple_name
    : identifier                                                                 { }
    ;

selected_name
    : prefix '.' suffix                                                          { }
    ;

prefix
    : name                                                                       { }
    ;

suffix
    : selected_name                                                              { }
    | all                                                                        { }
    ;


-- 7. Expressions

-- 7.1 Relations

expression
    : relation                                                                   { }
    | relation nand relation                                                     { }
    | relation nor relation                                                      { }
    | rel_ands                                                                   { }
    | rel_ors                                                                    { }
    | rel_xors                                                                   { }
    | rel_xnors                                                                  { }
    ;

rel_ands
    : relation and relation                                                      { }
    | relation and rel_ands                                                      { }
    ;

rel_ors
    : relation or relation                                                       { }
    | relation or rel_ors                                                        { }
    ;

rel_xors
    : relation xor relation                                                      { }
    | relation xor rel_xors                                                      { }
    ;

rel_xnors
    : relation xnor relation                                                     { }
    | relation xnor rel_xnors                                                    { }
    ;

relation
    : shift_expression relational_operator shift_expression                      { }
    | shift_expression                                                           { }
    ;

shift_expression
    : simple_expression                                                          { }
    ;

simple_expression
    : term                                                                       { }
    ;

term
    : factor                                                                     { }
    ;

factor
    : primary                                                                    { }
    | not primary                                                                { }
    ;

primary
    : name                                                                       { }
    | literal_nt                                                                 { }
    | '(' expression ')'                                                         { }
    ;


-- 7.2 Relational operators

relational_operator
    : '='                                                                        { }
    | '/='                                                                       { }
    | '<'                                                                        { }
    | '>'                                                                        { }
    | '<='                                                                       { }
    | '>='                                                                       { }
    ;


-- 7.3 Literals

literal_nt
    : numeric_literal                                                            { }
    | enumeration_literal                                                        { }
    ;

numeric_literal
    : abstract_literal                                                           { }
    ;


-- 8. Sequential statements

sequence_of_statements
    :                                                                            { }
    | sequential_statement sequence_of_statements                                { }
    ;

sequential_statement
    : wait_statement                                                             { }
    | signal_assignment_statement                                                { }
    | variable_assignment_statement                                              { }
    | if_statement                                                               { }
    | case_statement                                                             { }
    | null_statement                                                             { }
    ;


-- 8.1 Wait statement

wait_statement
    : label_nt ':' wait sensitivity_clause condition_clause ';'                  { }
    | label_nt ':' wait sensitivity_clause ';'                                   { }
    | label_nt ':' wait condition_clause ';'                                     { }
    | label_nt ':' wait ';'                                                      { }
    | wait sensitivity_clause condition_clause ';'                               { }
    | wait sensitivity_clause ';'                                                { }
    | wait condition_clause ';'                                                  { }
    | wait ';'                                                                   { }
    ;

label_nt
    : identifier                                                                 { }
    ;

sensitivity_clause
    : on sensitivity_list                                                        { }
    ;

sensitivity_list --Are signals
    : identifier                                                                 { }
    | identifier ',' sensitivity_list                                            { }
    ;

condition_clause
    : until condition                                                            { }
    ;


-- 8.2 Signal assignment statement

signal_assignment_statement
    : label ':' target '<=' waveform ';'                                         { }
    | target '<=' waveform ';'                                                   { }
    ;

target
    : name                                                                       { }
    ;

waveform
    : waveform_element                                                           { }
    ;

waveform_element
    : value_expression                                                           { }
    ;


-- 8.3 Variable assignment statement

variable_assignment_statement
    : label ':' target ':=' expression ';'                                       { }
    | target ':=' expression ';'                                                 { }
    ;

-- 8.4 If statement

if_statement
    : label ':' if condition then sequence_of_statements elsifs 
      else sequence_of_statements end if label ';'                               { }
    | label ':' if condition then sequence_of_statements elsifs 
      else sequence_of_statements end if ';'                                     { }
    | label ':' if condition then sequence_of_statements elsifs end if label ';' { }
    | label ':' if condition then sequence_of_statements elsifs end if ';'       { }
    | if condition then sequence_of_statements elsifs 
      else sequence_of_statements end if label ';'                               { }
    | if condition then sequence_of_statements elsifs 
      else sequence_of_statements end if ';'                                     { }
    | if condition then sequence_of_statements elsifs end if label ';'           { }
    | if condition then sequence_of_statements elsifs end if ';'                 { }
    ;

elsifs
    :                                                                            { }
    | elsif condition then sequence_of_statements elsifs                         { }
    ;


condition
    : boolean_expression                                                         { }
    ;


-- 8.5 Case statement

case_statement
    : label ':' case expression is case_statement_alternative
      case_statement_alternatives end case label ';'                             { }
    |  label ':' case expression is case_statement_alternative
      case_statement_alternatives end case ';'                                   { }
    | case expression is case_statement_alternative
      case_statement_alternatives end case label ';'                             { }
    | case expression is case_statement_alternative
      case_statement_alternatives end case ';'                                   { }
    ;

case_statement_alternative
    : when choices '=>' sequence_of_statements                                   { }
    ;

case_statement_alternatives
    :                                                                            { }
    | case_statement_alternative case_statement_alternatives                     { }
    ;


-- 8.6 Loop statement

loop_statement
    : label ':' while condition loop sequence_of_statements end loop label ';'   { }
    | label ':' while condition loop sequence_of_statements end loop ';'         { }
    | while condition loop sequence_of_statements end loop label ';'             { }
    | while condition loop sequence_of_statements end loop ';'                   { }
    ;


-- 8.7 Null statement

null_statement
    : label ':' null ';'                                                         { }
    | null ';'                                                                   { }
    ;


-- 9. Concurrent statements

concurrent_statement
    : block_statement                                                            { }
    | process_statement                                                          { }
    | concurrent_signal_assignment_statement                                     { }
    ;


-- 9.1 Block statement

block_statement
    : label ':' block is block_header block_declarative_part begin
      block_statement_part end block label ';'                                   { }
    | label ':' block is block_header block_declarative_part begin
      block_statement_part end block ';'                                         { }
    | label ':' block block_header block_declarative_part begin
      block_statement_part end block label ';'                                   { }
    | label ':' block block_header block_declarative_part begin
      block_statement_part end block ';'                                         { }
    ;

block_declarative_part
    :                                                                            { }
    |  block_declarative_item block_declarative_part                             { }
    ;

block_declarative_item
    : type_declaration                                                           { }
    | subtype_declaration                                                        { }
    | constant_declaration                                                       { }
    | signal_declaration                                                         { }
    | use_clause                                                                 { }
    ;

block_statement_part
    :                                                                            { }
    | concurrent_statement block_statement_part                                  { }
    ;


-- 9.2 Process statement

-- A. Unmentioned bindings

identifier
    : id                                                                         { }
    ;

label
    : identifier                                                                 { }
    ;


{

}
