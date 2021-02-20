/****************************************************/
/* File: tiny.y                                     */
/* The TINY Yacc/Bison specification file           */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
%{
#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *
static char * savedName; /* for use in assignments */
static int savedLineNo;  /* ditto */
static int savedNum;
static TreeNode * savedTree; /* stores syntax tree for later return */
static int yylex(void); // added 11/2/11 to ensure no conflict with lex

%}

%token WHILE RETURN INT VOID LE GT GE NE LBRACE RBRACE LCURLY RCURLY COMMA
%token IF THEN ELSE END REPEAT UNTIL READ WRITE
%token ID NUM
%token ASSIGN EQ LT PLUS MINUS TIMES OVER LPAREN RPAREN SEMI
%token ERROR

%nonassoc NO_ELSE
%nonassoc ELSE

%% /* Grammar for TINY */

program              : declaration_list
		          { savedTree = $1;}
	             ;
declaration_list     : declaration_list declaration
			  { YYSTYPE t = $1;
			    if (t != NULL)
                            { while (t->sibling != NULL)
                                 t = t->sibling;
                              t->sibling = $2;
                              $$ = $1; }
                              else $$ = $2;
                          }
		     | declaration { $$ = $1; }
	             ;
declaration	     : var_declaration { $$ = $1; }
		     | fun_declaration { $$ = $1; }
		     ;
saveName	     : ID
                 	{ savedName = copyString(tokenString);
                   	  savedLineNo = lineno;
                 	}
            	     ;
saveNumber  	     : NUM
                	{ savedNum = atoi(tokenString);
                  	  savedLineNo = lineno;
                	}
            	     ;
var_declaration      : type_specifier saveName SEMI
                 	{ $$ = newDeclNode(VarK);
                  	  $$->child[0] = $1;
                 	  $$->lineno = lineno;
                   	  $$->attr.name = savedName;
                 	}
        	     | type_specifier saveName LBRACE saveNumber RBRACE SEMI
                 	{ $$ = newDeclNode(VarArrK);
                   	  $$->child[0] = $1;
                   	  $$->lineno = lineno;
                   	  $$->attr.arr.name = savedName;
                   	  $$->attr.arr.size = savedNum;
                	}
            	     ;
type_specifier       : INT
			{ $$ = newTypeNode(TypeNameK);
                   	  $$->attr.type = INT;
                 	}
		     | VOID
			{ $$ = newTypeNode(TypeNameK);
                   	  $$->attr.type = VOID;
                 	}
            	     ;
fun_declaration	     : type_specifier saveName
                 	{ $$ = newDeclNode(FuncK);
                   	  $$->lineno = lineno;
                   	  $$->attr.name = savedName;
                 	}
              	       LPAREN params RPAREN compound_stmt
                 	{ $$ = $3;
                   	  $$->child[0] = $1;
                   	  $$->child[1] = $5;
                   	  $$->child[2] = $7;
                 	}
           	     ;
params		     : param_list  { $$ = $1; }
		     | type_specifier
                 	{ $$ = newParamNode(SingleParamK);
                   	  $$->child[0] = $1;
                   	  $$->attr.name = copyString("(null)");
                 	}
            	     ;
param_list  	     : param_list COMMA param
                 	{ YYSTYPE t = $1;
                  	 if(t != NULL)
                   	 { while (t->sibling != NULL)
                    	   	t = t->sibling;
                    	   t->sibling = $3;
                    	   $$ = $1; }
                    	 else $$ = $3;
                 	}
           	     | param { $$ = $1; }
		     ;
param		     : type_specifier saveName
                 	{ $$ = newParamNode(SingleParamK);
                   	  $$->child[0] = $1;
                   	  $$->attr.name = savedName;
                 	}
            	     | type_specifier saveName LBRACE RBRACE
                 	{ $$ = newParamNode(ArrParamK);
                   	  $$->child[0] = $1;
                   	  $$->attr.name = savedName;
                 	}
            	     ;
compound_stmt	     : LCURLY local_declarations statement_list RCURLY
                 	{ $$ = newStmtNode(CompK);
                 	  $$->child[0] = $2;
                 	  $$->child[1] = $3;
                 	}
            	     ;
local_declarations   : local_declarations var_declaration
                 	{ YYSTYPE t = $1;
                   	  if(t != NULL)
                   	  { while (t->sibling != NULL)
                            	t = t->sibling;
                     	    t->sibling = $2;
                     	    $$ = $1; }
                     	  else $$ = $2;
                 	}
            	     | { $$ = NULL; }
            	     ;
statement_list       : statement_list statement
                 	{ YYSTYPE t = $1;
                   	  if(t != NULL)
                   	  { while (t->sibling != NULL)
                            	t = t->sibling;
                     	    t->sibling = $2;
                     	    $$ = $1; }
                     	  else $$ = $2;
                 	}
            	     | { $$ = NULL; }
            	     ;
statement	     : expression_stmt { $$ = $1; }
		     | compound_stmt { $$ = $1; }
		     | selection_stmt { $$ = $1; }
		     | iteration_stmt { $$ = $1; }
		     | return_stmt { $$ = $1; }
		     ;
expression_stmt	     : expression SEMI { $$ = $1; }
		     | SEMI { $$ = NULL; }
		     ;
selection_stmt       : IF LPAREN expression RPAREN statement %prec NO_ELSE
                 	{ $$ = newStmtNode(IfK);
                   	  $$->child[0] = $3;
                   	  $$->child[1] = $5;
                   	  $$->child[2] = NULL;
                 	}
            	     | IF LPAREN expression RPAREN statement ELSE statement
                 	{ $$ = newStmtNode(IfEK);
                 	  $$->child[0] = $3;
                 	  $$->child[1] = $5;
                 	  $$->child[2] = $7;
                	}
            	     ;
iteration_stmt       : WHILE LPAREN expression RPAREN statement
                 	{ $$ = newStmtNode(IterK);
                   	  $$->child[0] = $3;
                  	  $$->child[1] = $5;
                 	}
            	     ;
return_stmt    	     : RETURN SEMI
                 	{ $$ = newStmtNode(RetK);
                   	  $$->child[0] = NULL;
                 	}
            	     | RETURN expression SEMI
                 	{ $$ = newStmtNode(RetK);
                   	  $$->child[0] = $2;
                 	}
            	     ;
expression           : var ASSIGN expression
                 	{ $$ = newExpNode(AssignK);
                  	  $$->child[0] = $1;
                  	  $$->child[1] = $3;
                 	}
            	     | simple_expression { $$ = $1; }
            	     ;
var         	     : saveName
                 	{ $$ = newExpNode(IdK);
                 	  $$->attr.name = savedName;
                 	}
            	     | saveName
                 	{ $$ = newExpNode(ArrIdK);
                 	  $$->attr.name = savedName;
                 	}
              	       LBRACE expression RBRACE
                 	{ $$ = $2;
                  	  $$->child[0] = $4;
                 	}
            	     ;
simple_expression    : additive_expression relop additive_expression
			{ $$ = $2;
			  $$->child[0] = $1;
			  $$->child[1] = $3;
			}
		     | additive_expression { $$ = $1; }
		     ;
relop		     : LE 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = LE;
			}
		     | LT 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = LT;
			}
		     | GE 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = GE;
			}
		     | GT 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = GT;
			}
		     | EQ 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = EQ;
			}
		     | NE 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = NE;
			}
		     ;
additive_expression  : additive_expression addop term
			{ $$ = $2;
			  $$->child[0] = $1;
			  $$->child[1] = $3;
			}
		     | term { $$ = $1; }
		     ;
addop		     : PLUS 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = PLUS;
			}
		     | MINUS 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = MINUS;
			}
		     ;
term		     : term mulop factor
			{ $$ = $2;
			  $$->child[0] = $1;
			  $$->child[1] = $3;
			}
		     | factor { $$ = $1; }
		     ;
mulop		     : TIMES 
			{ $$ = newExpNode(OpK);
			  $$->attr.op = TIMES;
			}
		     | OVER  
			{ $$ = newExpNode(OpK);
			  $$->attr.op = OVER;
			}
		     ;
factor      	     : LPAREN expression RPAREN { $$ = $2; }
            	     | var { $$ = $1; }
            	     | call { $$ = $1; }
            	     | saveNumber
                 	{ $$ = newExpNode(ConstK);
                   	  $$->attr.val = savedNum;
                 	}
            	     ;
call        	     : saveName
                 	{ $$ = newExpNode(CallK);
                	  $$->attr.name = savedName;
                 	}
              	       LPAREN args RPAREN
                 	{ $$ = $2;
                 	  $$->child[0] = $4;
                 	}
            	     ;
args        	     : arg_list  { $$ = $1; }
            	     | { $$ = NULL; }
            	     ;
arg_list    	     : arg_list COMMA expression
                 	{ YYSTYPE t = $1;
                   	  if(t != NULL)
                   	  { while (t->sibling != NULL)
                            	t = t->sibling;
                     	    t->sibling = $3;
                     	    $$ = $1; }
                     	  else $$ = $3;
                 	}
            	     | expression { $$ = $1; }
            	     ;

%%

int yyerror(char * message)
{ fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ return getToken(); }

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}

