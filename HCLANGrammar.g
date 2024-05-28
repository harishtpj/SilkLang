grammar HCLANGrammar;

program         : declaration* EOF ;
    
declaration     : classDecl
                | funDecl
                | varDecl
                | statement ;
    
classDecl       : 'class' IDENTIFIER ( ':' IDENTIFIER )?
                  '{' function* '}' ;
funDecl         : 'fun' function ;
varDecl         : 'let' IDENTIFIER ( ':=' expression )? ';' ;
    
statement       : exprStmt
                | forStmt
                | ifStmt
                | loopStmt
                | printStmt
                | returnStmt
                | whileStmt
                | importStmt
                | breakStmt
                | continueStmt
                | block ;

exprStmt        : expression ';' ;
forStmt         : 'for'  ( varDecl | exprStmt | ';' )
                           expression? ';'
                           expression? block ;
ifStmt          : 'if'  expression block ( 'else' block )? ;
loopStmt        : 'loop' statement ;
printStmt       : 'print' expression ';' ;
returnStmt      : 'return' expression? ';' ;
whileStmt       : 'while'  expression  block ;
importStmt      : 'import' ('std')? STRING;
breakStmt       : 'break' ';' ;
continueStmt    : 'continue' ';' ;
block           : '{' declaration* '}' ;



expression      : assignment ;

assignment      : ( call '.' )? IDENTIFIER (((':=' | '+=' | '-=' | '*=' | '/=' |
                                            '%=' | '^=') assignment)* | ('++' | '--'))
                | if_expr ;

if_expr         : logic_or ( 'if' logic_or 'else' logic_or)*;
logic_or        : logic_and ( 'or' logic_and )* ;
logic_and       : equality ( 'and' equality )* ;
equality        : comparison ( ( '!=' | '=' ) comparison )* ;
comparison      : term ( ( '>' | '>=' | '<' | '<=' ) term )* ;
term            : factor ( ( '-' | '+' ) factor )* ;
factor          : unary ( ( '^' | '/' | '*' ) unary )* ;

unary           : ( '!' | '-' ) unary | call ;
call            : primary ( '(' arguments? ')' | '.' IDENTIFIER )* ;
primary         : 'true' | 'false' | 'null' | 'self' |
                | NUMBER | STRING | IDENTIFIER | '(' expression ')'
                | 'super' '.' IDENTIFIER ;

function       : IDENTIFIER '(' parameters? ')' block ;
parameters     : IDENTIFIER ( ',' IDENTIFIER )* ;
arguments      : expression ( ',' expression )* ;


NUMBER         : DIGIT+ ( '.' DIGIT+ )? ;
STRING         : '"' .+ '"' ;
IDENTIFIER     : ALPHA ( ALPHA | DIGIT )* ;
ALPHA          : ('a'..'z''A'..'Z''_')+ ;
DIGIT          : ('0'..'9')+ ;