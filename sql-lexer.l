%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
 
#include "sql-parser.tab.h"
%}

%option noyywrap
%option header-file="lex.yy.h"

%%

"SELECT"    					            {return SELECT;}
"WHERE"     					            {return WHERE;}
"FROM"      					            {return FROM;}
"UPDATE"    				               	{return UPDATE;}
"INTO"      				            	{return INTO;}
"DELETE"    				            	{return DELETE;}
"DROP" 						                {return DROP;}
"TABLE"						                {return TABLE;}
"CREATE"					                {return CREATE;}
"VALUES"									{return VALUES;}
"INT"										{return INT;}
"FLOAT"										{return FLOAT;}
"BOOL"										{return BOOL;}
"TRUE"|"FALSE"|"true"|"false"				{yylval.str = strdup(yytext); return BOOL_type;}
"INSERT"					                {return INSERT;}        
"SET"						                {return SET;}
"AND"										{return AND;}
"OR"										{return OR;}
"NOT"										{return NOT;}
"*"											{return STAR;}
"+"											{return PLUS;}
"-"											{return MINUS;}
"VARCHAR"       				            {return VARCHAR;}
[a-zA-Z_][a-zA-Z0-9_]* 				 		{yylval.str = strdup(yytext); return IDENTIFIER; }
[0-9]+"."[0-9]+    				            {yylval.fnum=atof(yytext); return FLOAT_NUM;}
[0-9]+						                {yylval.num=atoi(yytext); return INT_NUM;}
['"][^'"]*['"]                              {yylval.str = strdup(yytext); return STRING_LITERAL;}
"<="										{return LESSER_EQUAL;}
">="										{return GREATER_EQUAL;}
"<"						                    {return LESSER;}
">"						                    {return GREATER;}
"="						                    {return EQUAL;}
","						                    {return COMMA;}
"!="										{return DIFFERENT_OF;}
"("											{return OPEN_PAR;}
")"											{return CLOSE_PAR;}
";"											{return SEMICOLON;}
\n                                          {;}
[ \t]      							        {;}
.						                    {printf("ERREUR LEXICALE : charactere inconnu '%c'\n", yytext[0]);}

%%
