%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define YYDEBUG 1
int productions=0; 
int indexes[1000]; 
%}

%token INT_CT
%token STRING_CT

%token ARRAY
%token CONST
%token DO
%token FOR
%token ELSE
%token IF
%token INT
%token READ
%token THEN
%token VAR
%token WHILE
%token WRITE
%token TRUE
%token FALSE
%token IDENTIFIER
%token BOOLEAN
%token INTEGER
%token REAL
%token CHAR
%token DOUBLE

%left '+' 
%left '-'
%left '*' 
%left '/'
%left '%'


%%

program : VAR decllist cmpdstmt {productions++;indexes[productions]=0;} 
        ;
decllist : declaration {productions++;indexes[productions]=1;}
        | declaration ';' decllist {productions++;indexes[productions]=2;}
        ;
declaration : IDENTIFIER ':' type {productions++;indexes[productions]=3;}
        ;

type : type1 {productions++;indexes[productions]=4;}
		| arraydecl {productions++;indexes[productions]=5;}
		;
type1 : BOOLEAN {productions++;indexes[productions]=6;}
		| INTEGER {productions++;indexes[productions]=7;}
		| REAL {productions++;indexes[productions]=8;}
		| CHAR {productions++;indexes[productions]=9;}
		| DOUBLE {productions++;indexes[productions]=10;}
        ;
arraydecl : ARRAY '[' INT_CT ']' 'OF' type1 {productions++;indexes[productions]=11;}

		;
cmpdstmt : '{' stmtlist '}' {productions++;indexes[productions]=12;}
        ;
stmtlist : stmt {productions++;indexes[productions]=13;}
		| stmt ';' stmtlist {productions++;indexes[productions]=14;}
		;
stmt : simplstmt {productions++;indexes[productions]=15;}
		| structstmt {productions++;indexes[productions]=16;}
		;
simplstmt : assignstmt {productions++;indexes[productions]=17;}
		      | iostmt {productions++;indexes[productions]=18;}
		      ;
assignstmt : IDENTIFIER ':=' expression {productions++;indexes[productions]=19;}
	   | IDENTIFIER'['IDENTIFIER']' ':=' expression {productions++;indexes[productions]=20;}
        ;

expression : expression '+' term {productions++;indexes[productions]=21;}
	| term {productions++;indexes[productions]=22;}
        | expression '-' term {productions++;indexes[productions]=23;}
        ;
term : term '*' factor {productions++;indexes[productions]=24;}
	| factor  {productions++;indexes[productions]=25;}
        | term '/' factor {productions++;indexes[productions]=26;}
        | term '%' factor {productions++;indexes[productions]=27;}
        ;
factor: '(' expression ')'  {productions++;indexes[productions]=28;}
        | IDENTIFIER {productions++;indexes[productions]=29;}
	| INT_CT {productions++;indexes[productions]=30;}
	| STRING_CT  {productions++;indexes[productions]=31;}
       
        ;
iostmt : READ '(' IDENTIFIER ')' {productions++;indexes[productions]=32;}
	| WRITE '(' IDENTIFIER ')' {productions++;indexes[productions]=33;}
        ;

structstmt : cmpdstmt {productions++;indexes[productions]=34;}
        | ifstmt {productions++;indexes[productions]=35;}
        | whilestmt {productions++;indexes[productions]=36;}
        | dowhilestmt {productions++;indexes[productions]=37;}
        | forstmt {productions++;indexes[productions]=38;}
        ;

        
ifstmt : IF '(' condition ')' stmt '[' ELSE stmt ']' {productions++;indexes[productions]=39;}

        ;
whilestmt : WHILE  '(' condition ')' stmt {productions++;indexes[productions]=40;}
        ;
dowhilestmt : DO stmt WHILE '(' condition ')' {productions++;indexes[productions]=41;}
        ;
forstmt : FOR  '(' condition ')' stmt {productions++;indexes[productions]=42;}
		;
condition : expression  relation expression {productions++;indexes[productions]=43;}
        ;
relation : '<' {productions++;indexes[productions]=44;}
        | '<=' {productions++;indexes[productions]=45;}
        | '=' {productions++;indexes[productions]=46;}
        | '>=' {productions++;indexes[productions]=47;}
        | '>' {productions++;indexes[productions]=48;}
        ;

%%


yyerror(char *s)
{
  printf("Error after %d productions, at production: %d\n",productions, indexes[productions]); 
}

extern FILE *yyin;

main(int argc, char **argv)
{

  if(argc>1) yyin = fopen(argv[1], "r");
  if((argc>2)&&(!strcmp(argv[2],"-d"))) yydebug = 1;
  if(!yyparse()){
    fprintf(stderr,"\tProgram syntactic correct, %d productions used\n",productions); 
    for(int i=1;i<=productions;i++) 
      fprintf(stderr,"\tProduction %d was used\n",indexes[i]); 


  }
}
