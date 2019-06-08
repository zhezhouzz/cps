structure Tokens = Tokens
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type lexresult = (svalue,pos) token
val line = ref 1
val debugFlag = ref false
val eof = fn () => Tokens.EOF(!line,!line)
val debug = fn s => if (!debugFlag) then print s else ()
(*
  Untyped_TOKENS defined using term declaration in grm
 <INITIAL>{integer} => (debug ("var: "^yytext^"\n"); Tokens.NUMB(yytext,!line,!line));
 (* <INITIAL>{optsign}{integer}({frac}{exp}?|{frac}?{exp}) => (debug ("var: "^yytext^"\n"); Tokens.NUMB(yytext,!line,!line)); *)
*)
%%
%header (functor UntypedLexFun (structure Tokens : Untyped_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
variable={alpha}({alpha}|{digit})*;
eol=("\n"|"\013\n"|"\013");
ws=[\ \t];
%%
<INITIAL>{eol} => (line := (!line)+1; lex());
<INITIAL>{ws}+ => (debug "whitespace"; lex());
<INITIAL>("\\") => (Tokens.LAMBDA(!line,yypos));
<INITIAL>(".") => (Tokens.DOT(!line,yypos));
<INITIAL>("@") => (Tokens.APP(!line,yypos));

<INITIAL>("(") => (debug "lparen\n"; Tokens.LPAREN(!line,yypos));
<INITIAL>(")") => (debug "rparen\n"; Tokens.RPAREN(!line,yypos));

<INITIAL>{variable} => (debug ("var: "^yytext^"\n"); Tokens.VAR(yytext,!line,yypos));
