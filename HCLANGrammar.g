grammar HCLANGrammar;

program         : declaration* EOF ;
    
declaration     : classDecl
                | funDecl
                | varDecl
                | statement ;
    
classDecl       : 'class' IDENTIFIER ( '{' IDENTIFIER )?
                  '{' function* '}' ;
funDecl         : 'fun' function ;
varDecl         : 'let' IDENTIFIER ( ':=' expression )? ';' ;
    
statement       : exprStmt
                | forStmt
                | ifStmt
                | loopStmt
                | printStmt
                | printLnStmt
                | returnStmt
                | whileStmt
                | importStmt
                | block ;

exprStmt        : expression ';' ;
forStmt         : 'for' '(' ( varDecl | exprStmt | ';' )
                           expression? ';'
                           expression? ')' statement ;
ifStmt          : 'if'  expression statement ( 'else' statement )? ;
loopStmt        : 'loop' statement ;
printStmt       : 'print' expression ';' ;
printLnStmt     : 'println' expression ';' ;
returnStmt      : 'return' expression? ';' ;
whileStmt       : 'while'  expression  statement ;
importStmt      : 'import' ('std')? STRING;
block           : '{' declaration* '}' ;



expression      : assignment ;

assignment      : ( call '.' )? IDENTIFIER ':=' assignment
                | logic_or ;

logic_or        : logic_and ( 'or' logic_and )* ;
logic_and       : equality ( 'and' equality )* ;
equality        : comparison ( ( '!=' | '=' ) comparison )* ;
comparison      : term ( ( '>' | '>=' | '<' | '<=' ) term )* ;
term            : factor ( ( '-' | '+' ) factor )* ;
factor          : unary ( ( '^' | '/' | '*' ) unary )* ;

unary           : ( '!' | '-' ) unary | call ;
call            : primary ( '(' arguments? ')' | '.' IDENTIFIER )* ;
primary         : 'true' | 'false' | 'null' | 'self' | 'break' | 'continue'
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