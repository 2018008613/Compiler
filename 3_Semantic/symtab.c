/****************************************************/
/* File: symtab.c                                   */
/* Symbol table implementation for the TINY compiler*/
/* (allows only one symbol table)                   */
/* Symbol table is implemented as a chained         */
/* hash table                                       */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"
//#include <globals.h>

/* SIZE is the size of the hash table */
//#define SIZE 211
#define SCOPESIZE 2000

/* SHIFT is the power of two used as multiplier
   in hash function  */
#define SHIFT 4

/* the hash function */
static int hash ( char * key )
{ int temp = 0;
  int i = 0;
  while (key[i] != '\0')
  { temp = ((temp << SHIFT) + key[i]) % SIZE;
    ++i;
  }
  return temp;
}



ScopeList scopes[SCOPESIZE];
ScopeList scopeNow = NULL;
int scopeNum = 0;

void st_insert(char * name, ExpType type, int lineno, TreeNode* treeNode)
{
	int h = hash(name);
	ScopeList thisScope = scopeNow;
	BucketList l = thisScope -> bucket[h];
	while ((l != NULL) && (strcmp(name,l->name) != 0))
		l = l->next;
	if (l == NULL) /* variable not yet in table */
	{
		l = (BucketList) malloc(sizeof(struct BucketListRec));
		//l->name = name;
		strcpy(l->name, name);
		l->lines = (LineList) malloc(sizeof(struct LineListRec));
		l->lines->lineno = lineno;
		l->treeNode = treeNode;
		l->memloc = 0;
		l->lines->next = NULL;
		l->type = type;
		l->next = thisScope -> bucket[h];
		thisScope -> bucket[h] = l;
	}
	else /* found in table, so just add line number */
	{ 
		LineList t = l->lines;
		while (t->next != NULL)
			t = t->next;
		t->next = (LineList) malloc(sizeof(struct LineListRec));
		t->next->lineno = lineno;
		t->next->next = NULL;
	}
}



BucketList st_lookup ( char * scope, char * name )
{
	ScopeList thisScope = find_scope(scope);
	int h = hash(name);
	while(thisScope != NULL)
	{
		BucketList l =  thisScope->bucket[h];
		while ((l != NULL) && (strcmp(name,l->name) != 0))
			l = l->next;
		if (l != NULL)
			return l;
		thisScope = thisScope->parent;
	}
	return NULL;
}

BucketList st_lookup_excluding_parent ( char * scope, char * name )
{
	ScopeList thisScope = find_scope(scope);
	if (thisScope == NULL)
		return NULL;
	int h = hash(name);
	BucketList l =  thisScope->bucket[h];
	while ((l != NULL) && (strcmp(name,l->name) != 0))
		l = l->next;
	if (l != NULL)
		return l;
	return NULL;
}

ScopeList find_scope ( char * scope)
{
	for (int i = 0; i < scopeNum; i++)
		if ( strcmp(scope, scopes[i]->name) == 0)
			return scopes[i];
	return NULL;
}

ScopeList addScope( char * name )
{
	ScopeList newScope = (ScopeList)malloc(sizeof(struct ScopeListRec));
	//newScope->name = name;
	strcpy(newScope->name,name);
	newScope->parent = scopeNow;
	scopeNow = newScope;
	scopes[scopeNum++] = newScope;
	return newScope;	
}

ScopeList goParent()
{
	scopeNow = scopeNow->parent;
	return scopeNow;
}

ScopeList updateScopeNow(char * name)
{
	scopeNow = find_scope(name);
	return scopeNow;
}

/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab(FILE * listing)
{
	int i, j;
	fprintf(listing,"< Symbol Table >\n");
	fprintf(listing,"Variable Name  Variable Type  Scope Name  Line Numbers\n");
	fprintf(listing,"-------------  -------------  ----------  ------------\n");

	for (i = 0; i < scopeNum; i++)
	{
		ScopeList thisScope = scopes[i];
		BucketList * hashTable = thisScope->bucket;

		for (j = 0; j < SIZE; j++)
		{
			if(hashTable[j] != NULL)
			{
				BucketList bl = hashTable[j];
				TreeNode * node = bl->treeNode;

				while(bl != NULL)
				{
					LineList ll = bl->lines;
					fprintf(listing,"%-15s",bl->name);

					switch (node->nodekind)
					{
						case DeclK:
							switch (node->kind.decl)
							{
								case FuncK:
									fprintf(listing,"%-15s","Function");
									break;
								case VarK:
									switch (node->type)
									{
										case Void:
											fprintf(listing,"%-15s","Void");
											break;
										case Integer:
											fprintf(listing,"%-15s","Integer");
											break;
										default:
											break;
									}
									break;
								case VarArrK:
									fprintf(listing,"%-15s","IntegerArray");
									break;
								default:
									break;
							}
							break;
						case ParamK:
							switch (node->kind.param)
							{
								case ArrParamK:
									fprintf(listing,"%-15s","IntegerArray");
									break;
								case SingleParamK:
									fprintf(listing,"%-15s","Integer");
									break;
								default:
									break;
							}
							break;
						default:
							break;
				}

				fprintf(listing,"%-12s",thisScope->name);
				while(ll != NULL)
				{
					fprintf(listing,"%4d",ll->lineno);
					ll = ll->next;
				}
				fprintf(listing,"\n");
          
				bl = bl->next;
			}
		}
	}
  }
}
