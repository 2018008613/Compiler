/****************************************************/
/* File: analyze.c                                  */
/* Semantic analyzer implementation                 */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "analyze.h"
#include "util.h"

/* counter for variable memory locations */
static int location = 0;

ScopeList curScope = NULL;
int existScope = 0;
char funcname[31];

/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ if (t != NULL)
  { preProc(t);
    { int i;
      for (i=0; i < MAXCHILDREN; i++)
        traverse(t->child[i],preProc,postProc);
    }
    postProc(t);
    traverse(t->sibling,preProc,postProc);
  }
}

/* nullProc is a do-nothing procedure to 
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void nullProc(TreeNode * t)
{ if (t==NULL) return;
  else return;
}

static void undeclaredError(char * name, int lineno)
{
	fprintf(listing,"Error: Undeclared %s at line %d\n",name, lineno);
	Error = TRUE;
}

static void typeError(int lineno, char * ch)
{ fprintf(listing,"Error: %s at line %d\n",ch, lineno);
  Error = TRUE;
}

static void redefinedError(char * name, int lineno)
{ 
	fprintf(listing,"Error: Redefined %s at line %d\n", name, lineno);
	Error = TRUE;
}

static void funcGlobalDeclError(char * name, int lineno)
{
	fprintf(listing,"Error: Function %s not declared global at line %d\n",name, lineno);
	Error = TRUE;
}

static void voidVarError(char * name, int lineno)
{
	fprintf(listing,"Error: Variable %s's Type cannot be Void at line %d\n",name, lineno);
	Error = TRUE;
}

static void init()
{
	curScope = addScope("global");

	TreeNode * func;
	TreeNode * typeSpec;
	TreeNode * param;
	TreeNode * compStmt;

	//input
	typeSpec = newTypeNode(FuncK);
	typeSpec->attr.type = INT;

	compStmt = newStmtNode(CompK);
	compStmt->child[0] = NULL;
	compStmt->child[1] = NULL;

	func = newDeclNode(FuncK);
	func->type = Integer;
	func->lineno = 0;
	func->attr.name = "input";
	func->child[0] = typeSpec;
	func->child[1] = NULL;
	func->child[2] = compStmt;

	st_insert("input", Integer, 0, func);

	//output


	typeSpec = newTypeNode(FuncK);
	typeSpec->attr.type = VOID;
  

	param = newParamNode(SingleParamK);   // single integer parameter
	param->attr.name = "arg";
	param->type = Integer;
	param->child[0] = newTypeNode(FuncK);
	param->child[0]->attr.type = INT;

	compStmt = newStmtNode(CompK);
	compStmt->child[0] = NULL;  // no local variable
	compStmt->child[1] = NULL;  // no statement

	func = newDeclNode(FuncK);
	func->type = Void;
	func->lineno = 0;
	func->attr.name = "output";
	func->child[0] = typeSpec;
	func->child[1] = param;
	func->child[2] = compStmt;

	st_insert("output", Void, 0, func);
}

/* Procedure insertNode inserts 
 * identifiers stored in t into 
 * the symbol table 
 */
static void insertNode( TreeNode * t)
{ 
	switch (t->nodekind)
	{
		case DeclK:
			switch (t->kind.decl)
			{
				case VarK:
        			case VarArrK:
					{
						ExpType thisType;
						if (t->kind.decl == VarK)
						{
							thisType = Integer;
							t->type = Integer;
						}
						else
						{
							thisType = INTARR;
							t->type = INTARR;
						}
						if (st_lookup_excluding_parent(curScope->name, t->attr.name) != NULL)
						{
							redefinedError(t->attr.name, t->lineno);
							break;
						}
						st_insert(t->attr.name, thisType, t->lineno, t);
					}
          				break;
				case FuncK:
					strcpy(funcname, t->attr.name);
					if (st_lookup_excluding_parent(curScope->name, t->attr.name) != NULL)
					{
						redefinedError(t->attr.name, t->lineno);
						break;
					}
					if (strcmp(curScope->name, "global") != 0)
					{
						funcGlobalDeclError(t->attr.name, t->lineno);
						break;
					}

     

					
					st_insert(t->attr.name, t->type, t->lineno, t);
					curScope = addScope(funcname);
					existScope = 1;
					switch (t->child[0]->attr.type)
					{
						case INT:
							t->type = Integer;
							break;
						case VOID:
						default:
							t->type = Void;
							break;
					}
					break;



				default:
					break;
			}
			break;
		case ParamK:
			if (t->child[0]->attr.type == VOID)
				break;
			if (st_lookup(curScope->name, t->attr.name) == NULL)
			{
				ExpType thisType;				
				if(t->kind.param == SingleParamK)
					thisType = Integer;
				else
					thisType = INTARR;
				t->type = thisType;
				st_insert(t->attr.name, thisType, t->lineno, t);
			}
			break;
		case StmtK:
			if (t->kind.stmt == CompK)
			{
				if (existScope)
					existScope = 0;
				else
					curScope = addScope(funcname);
				strcpy(t->attr.scopeName, curScope->name);
			}
			break;
		case ExpK:
			switch (t->kind.exp)
			{
				case IdK:
				case ArrIdK:
				case CallK:
				{
					BucketList l = st_lookup(curScope->name, t->attr.name);
					if (l == NULL)
						undeclaredError(t->attr.name, t->lineno);
					else
					{
						if (t->kind.exp == CallK)
							t->type = l->type;
						st_insert(t->attr.name, t->type, t->lineno, t);
					}
				}
					break;
				default:
					break;
			}
			break;
		default:
			break;
	} 
}

static void afterInsertNode(TreeNode * t)
{
	if (t->nodekind == StmtK && t->kind.stmt == CompK)
		curScope = goParent();
}

/* Function buildSymtab constructs the symbol 
 * table by preorder traversal of the syntax tree
 */
void buildSymtab(TreeNode * syntaxTree)
{ init();
  traverse(syntaxTree,insertNode,afterInsertNode);
  if (TraceAnalyze)
  { fprintf(listing,"\nSymbol table:\n\n");
    printSymTab(listing);
  }
  curScope = goParent();
}

static void beforeCheckNode(TreeNode * t)
{
	switch (t->nodekind)
	{
		case DeclK:
			if (t->kind.decl == FuncK)
				strcpy(funcname, t->attr.name);
			break;
		case StmtK:
			if (t->kind.stmt == CompK)
				curScope = updateScopeNow(t->attr.scopeName);
			break;
		default:
			break;
	}
}

/* Procedure checkNode performs
 * type checking at a single tree node
 */
static void checkNode(TreeNode * t)
{

	switch (t->nodekind)
	{
		case DeclK:
			//strcpy(funcname, t->attr.name);
			//curScope = updateScopeNow(funcname);
			//existScope = 1;
			if (t->kind.decl == VarK || t->kind.decl == VarArrK)
			{
				if (t->child[0]->attr.type == VOID)
				{           
					if (t->kind.decl == VarK)
						voidVarError(t->attr.name, t->lineno);
					else
						voidVarError(t->attr.arr.name, t->lineno);
				}
			}
			break;
		case StmtK:
			switch (t->kind.stmt)
			{
				case CompK:
					curScope = goParent();
					break;
				case IfK:
				case IfEK:
					if (t->child[0] == NULL)
						typeError(t->lineno,"expected expression error");
					else if (t->child[0]->type == Void)
						typeError(t->child[0]->lineno,"if condition type error");
					break;
				case IterK:
					if (t->child[0] == NULL)
						typeError(t->lineno,"expected expression");
					else if (t->child[0]->type == Void)
						typeError(t->child[0]->lineno,"loop condition type error");
					break;
				case RetK:
					{
						TreeNode * retFunc = st_lookup(curScope->name, funcname)->treeNode;
						if ((retFunc->type == Void && t->child[0] != NULL) ||
(retFunc->type == Integer && (t->child[0] == NULL || t->child[0]->type == Void || t->child[0]->type == INTARR)))
							typeError(t->lineno,"return type error");
						//if (st_lookup(curScope->name, t->child[0]->attr.name) == NULL)
						//	undeclaredError(t->attr.name, t->lineno);
						break;
					}
				default:
					break;
			}
			break;
		case ExpK:
			switch (t->kind.exp)
			{
				case AssignK:
					if (t->child[0]->type == Void || t->child[1]->type == Void)
						typeError(t->child[0]->lineno,"variable type error");
					else if (t->child[0]->type == INTARR && t->child[0]->child[0] == NULL)
						typeError(t->child[0]->lineno,"variable type error");
					else if (t->child[1]->type == INTARR && t->child[1]->child[0] == NULL)
						typeError(t->child[0]->lineno,"variable type error");
					else 
						t->type = t->child[0]->type;
					break;

				case IdK:
				case ArrIdK:
					{
						BucketList l = st_lookup(curScope->name, t->attr.name);
						if (l == NULL)
							break;

						TreeNode * symbolNode = l->treeNode;

						if (t->kind.exp == ArrIdK)
						{
							if ((symbolNode->nodekind == DeclK && l->type != INTARR) || (symbolNode->nodekind == ParamK && l->type != INTARR))
								typeError(t->lineno, "expression error");
							else
								t->type = symbolNode->type;
						}
						else
							t->type = symbolNode->type;
						break;
					}

				case OpK:
					{
						ExpType lType, rType;
						TokenType op;

						lType = t->child[0]->type;
						rType = t->child[1]->type;
						op = t->attr.op;

						if(lType == INTARR && t->child[0]->child[0] != NULL)
							lType = Integer;
						if(rType == INTARR && t->child[1]->child[0] != NULL)
							rType = Integer;

						if (lType == Void || rType == Void)
							typeError(t->lineno,"void variable error");
						else if (lType != rType)
							typeError(t->lineno,"operands type error");
						else
							t->type = Integer;
						break;
					}
				case ConstK:
					t->type = Integer;
					break;

				case CallK:
					{
						BucketList l = st_lookup(curScope->name, t->attr.name);
						if (l == NULL)
							break;
						TreeNode * funcNode = l->treeNode;
						TreeNode * arg = t->child[0];
						TreeNode * param= funcNode->child[1];

						if (funcNode->kind.decl != FuncK)
						{
							typeError(t->lineno, "expression error");
							break;
						}

						while (arg != NULL)
						{
							if (param == NULL || arg->type == Void)
							{
								typeError(arg->lineno, "function call error");
								break;
							}
							ExpType pType = param->type;
							ExpType aType = arg->type;            
							if(aType == INTARR && arg->child[0] != NULL)
								aType = Integer;

							if (pType != aType)
							{
								typeError(arg->lineno, "function call error");
								break;
							}
							else
							{
								arg = arg->sibling;
								param = param->sibling;
							}
						}
						if (arg == NULL && param != NULL && param->child[0]->attr.type != VOID)
            						typeError(t->child[0]->lineno,"function call error");
          
						t->type = funcNode->type;
						break;
					}

				default:
					break;
			}
			break;
		default:
			break;
	}
}

/* Procedure typeCheck performs type checking 
 * by a postorder syntax tree traversal
 */
void typeCheck(TreeNode * syntaxTree)
{ 
  curScope = updateScopeNow("global");
  traverse(syntaxTree,beforeCheckNode,checkNode);
  curScope = goParent();
}
