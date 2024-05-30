
/*
    RUN: swipl -f parser.pl -t testAll

*/


/******************************************************************************/
/* Prolog Lab 3 parser                                  */
/******************************************************************************/

prog          --> prog_head, var_part, stat_part.

/******************************************************************************/
/* Program Header                                                             */
/******************************************************************************/
prog_head     --> program, id, leftp, input, comma, output, rightp, semicolon.

/******************************************************************************/
/* Var_part                                                                   */
/******************************************************************************/
var_part     --> var, var_dec_list.
var_dec_list --> var_dec | var_dec, var_dec_list.
var_dec      --> id_list, colon, type, semicolon.
id_list      --> id | id, comma, id_list.
type         --> integer | real | boolean.

/******************************************************************************/
/* Stat part                                                                  */
/******************************************************************************/
stat_part   --> begin, stat_list, end, dot.
stat_list   --> stat | stat, semicolon, stat_list.
stat        --> assign_stat.
assign_stat --> id, assign_op, expr.
expr        --> term | term, add, expr.
term        --> factor | factor, mult, term.
factor      --> leftp, expr, rightp | operand.
operand     --> id | number.


/******************************************************************************/
/* Keytok                                                                     */
/******************************************************************************/
program      -->    [256].  % program %
input        -->    [257].  % input % 
output       -->    [258].  % output % 
var          -->    [259].  % var %
integer      -->    [260].  % integer %
begin        -->    [261].  % begin %
end          -->    [262].  % end %
boolean      -->    [263].  % boolean %
real         -->    [264].  % real %
id           -->    [270].  % id %

leftp        -->    [40].  % ( %
rightp       -->    [41].  % ) %
mult         -->    [42].  % * %
add          -->    [43].  % + %
comma        -->    [44].  % , %
dot          -->    [46].  % . %
colon        -->    [58].  % : %
semicolon    -->    [59].  % ; %
assign_op    -->    [271]. % assign %
number       -->    [272]. % number %

unknwn       -->    [273]. % unknown characters % 
eof          -->    [275]. % end of file %



/******************************************************************************/
/* Lexer                                                                      */
/******************************************************************************/


/***********************************
I wanted to do it this way because
then the output will actually show
what 'symbol' (token name) that was matched,
instead of token numbers.

Unfortunately I did not make this work
***********************************/
/*
lexer([], []).
lexer([Head|Tail], [Tok|TailTok]) :- match(Head, Tok), lexer(Tail, TailTok).
match(Lex, Tok) :- Lex = 'program', Tok = program.
match(Lex, Tok) :- Lex = 'input'  , Tok = input.
match(Lex, Tok) :- Lex = 'output' , Tok = output.
match(Lex, Tok) :- Lex = 'var'    , Tok = var.
match(Lex, Tok) :- Lex = 'integer', Tok = integer.
match(Lex, Tok) :- Lex = 'real'   , Tok = real.
match(Lex, Tok) :- Lex = 'boolean', Tok = boolean.
match(Lex, Tok) :- Lex = 'begin'  , Tok = begin.
match(Lex, Tok) :- Lex = 'end'    , Tok = end.
match(Lex, Tok) :- Lex = ':='     , Tok = assign_op.
match(Lex, Tok) :- Lex = '('      , Tok = leftp.
match(Lex, Tok) :- Lex = ')'      , Tok = rightp.
match(Lex, Tok) :- Lex = '*'      , Tok = mult.
match(Lex, Tok) :- Lex = '+'      , Tok = add.
match(Lex, Tok) :- Lex = ','      , Tok = comma.
match(Lex, Tok) :- Lex = '.'      , Tok = dot.
match(Lex, Tok) :- Lex = ':'      , Tok = colon.
match(Lex, Tok) :- Lex = ';'      , Tok = semicolon.

% match(id) %
match(Lex, Tok) :- name(Lex, [Head|Tail]), char_type(Head, alpha), match_id(Tail), Tok = id.

% match(number) %
match(Lex, Tok) :- name(Lex, [Head|Tail]), char_type(Head, digit), match_number(Tail), Tok = number.

% EOF %
match(Lex, Tok) :- char_type(Lex, end_of_file), Tok = eof.

% Unknown characters %
match(Lex, Tok) :- char_type(Lex, ascii), Tok = unknwn.
*/
lexer([], []).
lexer([Head|Tail], [Tok|TailTok]) :- match(Head, Tok), lexer(Tail, TailTok).
match(Lex, Tok) :- Lex = 'program', Tok is 256.
match(Lex, Tok) :- Lex = 'input'  , Tok is 257.
match(Lex, Tok) :- Lex = 'output' , Tok is 258.
match(Lex, Tok) :- Lex = 'var'    , Tok is 259.
match(Lex, Tok) :- Lex = 'integer', Tok is 260.
match(Lex, Tok) :- Lex = 'real'   , Tok is 264.
match(Lex, Tok) :- Lex = 'boolean', Tok is 263.
match(Lex, Tok) :- Lex = 'begin'  , Tok is 261.
match(Lex, Tok) :- Lex = 'end'    , Tok is 262.
match(Lex, Tok) :- Lex = ':='     , Tok is 271.
match(Lex, Tok) :- Lex = '('      , Tok is 40.
match(Lex, Tok) :- Lex = ')'      , Tok is 41.
match(Lex, Tok) :- Lex = '*'      , Tok is 42.
match(Lex, Tok) :- Lex = '+'      , Tok is 43.
match(Lex, Tok) :- Lex = ','      , Tok is 44.
match(Lex, Tok) :- Lex = '.'      , Tok is 46.
match(Lex, Tok) :- Lex = ':'      , Tok is 58.
match(Lex, Tok) :- Lex = ';'      , Tok is 59.

% match(id) %
match(Lex, Tok) :- name(Lex, [Head|Tail]), char_type(Head, alpha), match_id(Tail), Tok is 270.

% match(number) %
match(Lex, Tok) :- name(Lex, [Head|Tail]), char_type(Head, digit), match_number(Tail), Tok is 272.

% EOF %
match(Lex, Tok) :- char_type(Lex, end_of_file), Tok is 275.

% Unknown characters %
match(Lex, Tok) :- char_type(Lex, ascii), Tok is 273.

match_id([]).
match_id([Head|Tail]) :- char_type(Head, alnum), match_id(Tail).
match_number([]).
match_number([Head|Tail]) :- char_type(Head, digit), match_number(Tail). 

/******************************************************************************/
/******************************* TEST FILES ************************************/
/******************************************************************************/

testph :-   prog_head([program, id(c), leftp, input, comma, output, rightp, semicolon], []).
testpr :-   program([program, id(c), leftp, input, comma, output, rightp, semicolon], []).


            %Thanks for help%
testAll :-  tell('parser.out'), 
            write('Testing OK programs'), nl, nl, 
            parseFiles(['testfiles/testok1.pas', 'testfiles/testok2.pas', 'testfiles/testok3.pas', 'testfiles/testok4.pas', 
            'testfiles/testok5.pas', 'testfiles/testok6.pas', 'testfiles/testok7.pas']), 
            write('Testing a-z programs'), nl, nl,
            parseFiles(['testfiles/testa.pas', 'testfiles/testb.pas', 'testfiles/testc.pas', 'testfiles/testd.pas', 'testfiles/teste.pas',
            'testfiles/testf.pas', 'testfiles/testg.pas', 'testfiles/testh.pas', 'testfiles/testi.pas', 'testfiles/testj.pas',
            'testfiles/testk.pas', 'testfiles/testl.pas', 'testfiles/testm.pas', 'testfiles/testn.pas', 'testfiles/testo.pas',
            'testfiles/testp.pas', 'testfiles/testq.pas', 'testfiles/testr.pas', 'testfiles/tests.pas', 'testfiles/testt.pas',
            'testfiles/testu.pas', 'testfiles/testv.pas', 'testfiles/testw.pas', 'testfiles/testx.pas', 'testfiles/testy.pas',
            'testfiles/testz.pas']),
            write('Testing fun programs'), nl, nl,
            parseFiles(['testfiles/fun1.pas', 'testfiles/fun2.pas', 'testfiles/fun3.pas', 'testfiles/fun4.pas', 'testfiles/fun5.pas']), nl, 
            write('Testing sem programs'), nl, nl,
            parseFiles(['testfiles/sem1.pas', 'testfiles/sem2.pas', 'testfiles/sem3.pas', 'testfiles/sem4.pas', 'testfiles/sem5.pas']),
            told.




parseFiles([]).
parseFiles([Head|Tail]) :-    
   write('Testing '), write(Head), nl,
   read_in(Head, Lex), 
   lexer(Lex, Tokens), write(Lex), nl, write(Tokens), nl, 
   parser(Tokens, []), nl,
   write(Head), write(' end of parse'), nl, nl,
   parseFiles(Tail). 

parser(Tokens, Res) :-
    (prog(Tokens, Res), Res = [], 
    write('Parse OK!'))
    ; 
    write('Parse Fail!'). 


/******************************************************************************/
/******************************************************************************/
/********************************* READER PART (cmreader.pl) ********************************/
/******************************************************************************/
/******************************************************************************/

read_in(File,[W|Ws]) :- see(File), get0(C), 
                        readword(C, W, C1), restsent(W, C1, Ws), nl, seen.

/******************************************************************************/
/* Given a word and the character after it, read in the rest of the sentence  */
/******************************************************************************/

restsent(W, _, [])         :- W = -1.                /* added EOF handling */
restsent(W, _, [])         :- lastword(W).
restsent(_, C, [W1 | Ws ]) :- readword(C, W1, C1), restsent(W1, C1, Ws).

/******************************************************************************/
/* Read in a single word, given an initial character,                         */
/* and remembering what character came after the word (NB!)                   */
/******************************************************************************/

readword(C, W, _)  :- C = -1, W = C.                    /* added EOF handling */
readword(C, W, C2) :- C = 58, get0(C1), readwordaux(C,W,C1,C2).
readword(C, W, C2) :- C<58, C>47, name(W, [C]), get0(C2).
readword(C, W, C1) :- single_character( C ), name(W, [C]), get0(C1).
readword(C, W, C2) :-
   in_word(C, NewC ),
   get0(C1),
   restword(C1, Cs, C2),
   name(W, [NewC|Cs]).

readword(_, W, C2) :- get0(C1), readword(C1, W, C2).

readwordaux(C,W,C1,C2) :- C1 = 61, name(W,[C,C1]), get0(C2).
readwordaux(C,W,C1,C2) :- C1 \= 61, name(W,[C]), C1 = C2.

restword(C, [NewC|Cs], C2) :-
   in_word(C, NewC),
   get0(C1),
   restword(C1, Cs, C2).

restword(C, [ ], C).

/******************************************************************************/
/* These characters form words on their own                                   */
/******************************************************************************/

single_character(40).                  /* ( */
single_character(41).                  /* ) */
single_character(42).                  /* + */
single_character(43).                  /* * */
single_character(44).                  /* , */
single_character(45).                  /* - */
single_character(59).                  /* ; */
single_character(58).                  /* : */
single_character(61).                  /* = */
single_character(46).                  /* . */

/******************************************************************************/
/* These characters can appear within a word.                                 */
/* The second in_word clause converts character to lower case                 */
/******************************************************************************/

in_word(C, C) :- C>96, C<123.             /* a b ... z */
in_word(C, L) :- C>64, C<91, L is C+32.   /* A B ... Z */
in_word(C, C) :- C>47, C<58.              /* 1 2 ... 9 */

/******************************************************************************/
/* These words terminate a sentence                                           */
/******************************************************************************/

lastword('.').

/******************************************************************************/
/* End of program                                                             */
/******************************************************************************/