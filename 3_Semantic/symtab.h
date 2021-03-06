/****************************************************/
/* File: symtab.h                                   */
/* Symbol table interface for the TINY compiler     */
/* (allows only one symbol table)                   */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#ifndef _SYMTAB_H_
#define _SYMTAB_H_

#include "globals.h"

/* the list of line numbers of the source 
 * code in which a variable is referenced
 */
typedef struct LineListRec
   { int lineno;
     struct LineListRec * next;
   } * LineList;

/* The record in the bucket lists for
 * each variable, including name, 
 * assigned memory location, and
 * the list of line numbers in which
 * it appears in the source code
 */
#define SIZE 211

typedef struct BucketListRec
   { char name[30];
     ExpType type;
     LineList lines;
     int memloc ; /* memory location for variable */
     TreeNode* treeNode;
     struct BucketListRec * next;
   } * BucketList;

typedef struct ScopeListRec
   { char name[30];
     BucketList bucket[SIZE];
     struct scopeListRec * parent;
   } * ScopeList;

/* Procedure st_insert inserts line numbers and
 * memory locations into the symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert(char * name, ExpType type, int lineno, TreeNode* treeNode);

/* Function st_lookup returns the memory 
 * location of a variable or -1 if not found
 */


BucketList st_lookup ( char * scope, char * name );
BucketList st_lookup_excluding_parent ( char * scope, char * name );

ScopeList find_scope ( char * scope);
ScopeList addScope( char * name );
ScopeList goParent();
ScopeList updateScopeNow(char * name);

/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab(FILE * listing);

#endif
