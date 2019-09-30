
let rec print_tok = function
  | [] -> ()
  | t :: ts -> 
      print_endline(Cnl_parse.Lexer_cnl.lex_token_to_string t) ; print_tok ts

let convert_toks s = 
 let _ = print_endline "*" in
 let toks = Cnl_parse.Lexer_cnl.lex_string s in
 let _ = print_tok toks in
()


let _ = convert_toks ("ABC DEF\"abc\"\\abc \\@#3.4 5+4.5-6$#\\qe"^
"d()[]{}|-> .\\mid\\tmid , := -> _\\alt|\\sub/ /-\\^ \\\\ \\lam"^
"\\lambder\\Pity\\forall\\exists44 X+Y*x-y33/x__rad75'_. hello "^
"her_22 ab.cd.ef 1.rt.. ww._ \\exists.more/ ab.4.5. %ignore\n \\"^
"-> ->. ++4.5 x''+y''=z''" )

(* output of dune runtest

       test1 alias test/runtest
*
ABC (WORD)
DEF (WORD)
"abc" (STRING)
\abc(CONTROL)
\@(CONTROL)
# (SYMBOL)
3.4 (DECIMAL)
5 (INTEGER)
+4.5 (DECIMAL)
-6 (INTEGER)
$# (SYMBOL)
(QED)
(L_PAREN)
(R_PAREN)
(L_BRACK)
(R_BRACK)
(L_BRACE)
(R_BRACE)
(MAPSTO)
(PERIOD)
(MID)
(TMID)
(COMMA)
(ASSIGN)
(ARROW)
(BLANK)
(ALT)
(ALT)
(APPLYSUB)
(SLASH)
(SLASHDASH)
(COERCION)
(LAMBDA)
(LAMBDA)
(LAMBDA)
(PITY)
\forall (QUANTIFIER)
\exists (QUANTIFIER)
44 (INTEGER)
X (VAR)
+ (SYMBOL)
Y (VAR)
* (SYMBOL)
x (VAR)
- (SYMBOL)
y33 (VAR)
(SLASH)
x__rad75'_ (VAR)
(PERIOD)
hello (WORD)
her_22 (ATOMIC)
ab.cd.ef (HIERARCHICAL)
1 (INTEGER)
.rt (FIELD)
.. (SYMBOL)
ww (WORD)
._ (SYMBOL)
\exists (QUANTIFIER)
.more (FIELD)
(SLASH)
ab (WORD)
(PERIOD)
4.5 (DECIMAL)
(PERIOD)
\-(CONTROL)
> (SYMBOL)
->. (SYMBOL)
++ (SYMBOL)
4.5 (DECIMAL)
x'' (VAR)
+ (SYMBOL)
y'' (VAR)
= (SYMBOL)
z'' (VAR)
(EOF)

 *)





