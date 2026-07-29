%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "lex.yy.h"
#include "sql-parser.tab.h"
int yylex(void);
int yyerror(const char *s);
extern int yyparse(void);
#define MAX_CMD_LEN 1024

%}

%code requires {
    typedef struct table {
        char table_name[20];
        int table_size;
        char table_column_name[20][20];
        char table_column_type[20][20];
        int table_column_num;
    } table;

    typedef struct insert_columns_return {
        char names[20][20];
        int number;
    } insert_columns_return;
    }
%code {
    table AllTables[100];
    int numOfTables = 0;
    int column_counter = 0;
	int parse_error = 0;
    int condition_counter = 0;
    char updated_columns[20][20];
    int updated_counter = 0; 
    int vars_counter=0;
    int operator_counter=0;
    int line_numbers = 1;
}



%union {
    char* str;
    int num;
    float fnum;
	insert_columns_return rtrn;
}

%token SELECT WHERE FROM UPDATE INTO DELETE DROP TABLE CREATE INSERT SET COMMA EQUAL GREATER LESSER LESSER_EQUAL GREATER_EQUAL 
%token VARCHAR VARCHAR_TYPE DIFFERENT_OF OPEN_PAR CLOSE_PAR SEMICOLON PLUS MINUS STAR AND OR NOT VALUES
%token <str> IDENTIFIER BOOL_type STRING_LITERAL 
%token <num> INT_NUM
%token <fnum> FLOAT_NUM
%token INT FLOAT BOOL

%type <num> columns_def column_def insert_values where_clause condition newvalue
%type <str> data_type select_list vars columns
%type <rtrn> insert_columns

%%
program: 
       | program command
       ;

command: create_table
       | select_command
       | insert_command
       | update_command
       | delete_command
       | drop_command
	   | error SEMICOLON { printf("Veuillez corriger l'erreur et reessayer.\n"); yyerrok; }
       ;

value    :INT_NUM
		 |FLOAT_NUM
		 |STRING_LITERAL
		 |BOOL_type
		;

val_iden  : value
          |IDENTIFIER
          ;

vars:   IDENTIFIER{ $$ = strdup($1);
                    vars_counter=1; 
                    }
        | vars COMMA IDENTIFIER {
            vars_counter++; 
            char *result = malloc(strlen($1) + strlen($3) + 3);
            sprintf(result, "%s, %s", $1, $3);
            free($1);
            $$ = result;
        }
        ;

select_list :STAR  {$$=strdup("*");}
    		| vars {$$=strdup($1);}
    		;

condition	:val_iden GREATER val_iden          {condition_counter++;}
		    |val_iden EQUAL val_iden            {condition_counter++;}
		    |val_iden LESSER val_iden           {condition_counter++;}
		    |val_iden GREATER_EQUAL val_iden    {condition_counter++;}
		    |val_iden LESSER_EQUAL val_iden     {condition_counter++;}

conditions  :condition
			|conditions AND condition        {operator_counter++;}
			|conditions OR condition         {operator_counter++;}
			|NOT conditions                  {operator_counter++;}
			|OPEN_PAR conditions CLOSE_PAR
		    ;
newvalue    : IDENTIFIER EQUAL value
            {
            for (int i = 0; i < updated_counter; i++) {
            if (strcmp(updated_columns[i], $1) == 0) {
            printf("ERREUR SEMANTIQUE: La colonne '%s' est declaree plusieurs fois.\n", $1);
            YYABORT;
            }
            }
            strcpy(updated_columns[updated_counter], $1);
            updated_counter++;
            $$ = updated_counter;
            }
            | newvalue COMMA IDENTIFIER EQUAL value
            {
            for (int i = 0; i < updated_counter; i++) {
            if (strcmp(updated_columns[i], $3) == 0) {
            printf("ERREUR SEMANTIQUE: La colonne '%s' est declaree plusieurs fois.\n", $3);
            YYABORT;
            }
            }
            strcpy(updated_columns[updated_counter], $3);
            updated_counter++;
            $$ = updated_counter;
            }
            ;
data_type:INT      { $$ = strdup("INT"); }
    	 |FLOAT    { $$ = strdup("FLOAT"); }
    	 |VARCHAR OPEN_PAR INT_NUM CLOSE_PAR { 
         char* result = malloc(20);
         sprintf(result, "VARCHAR(%d)", $3);
         $$ = result;
    }
    | BOOL       { $$ = strdup("BOOL"); }
    ;

where_clause:                    {$$=0;}  
			|WHERE conditions    {$$=1;}
			;

columns  :IDENTIFIER { $$ = strdup($1); } 
		 |IDENTIFIER COMMA columns { 
            char* result = malloc(strlen($1) + strlen($3) + 3);
            sprintf(result, "%s, %s", $1, $3);
            $$ = result;
            free($3); 
         }
		 ;
insert_values  :value				        {$$=1;}
			   |value COMMA insert_values   {$$=1+$3;}
			   ;
insert_columns: IDENTIFIER {
                strcpy($$.names[0], $1); 
                $$.number = 1;
                }
                | insert_columns COMMA IDENTIFIER {
                strcpy($$.names[$1.number], $3);
                $$.number = $1.number + 1;
                for(int i = 0; i < $1.number; i++) {
                strcpy($$.names[i], $1.names[i]);
                }
                }
                ;

column_def  : IDENTIFIER data_type
            {
	        if (column_counter >= 20) {
            printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : Trop de colonnes (max 20)\n",line_numbers);
            YYABORT;
            }
	        for(int i = 0; i < column_counter; i++) {
            if(strcmp(AllTables[numOfTables].table_column_name[i], $1) == 0) {
                printf("ERREUR SEMANTIQUE DANS LA LIGNE %d: Colonne '%s' declaree plusieurs fois dans cette table.\n",line_numbers - 1,$1);
                YYABORT;
            }
            }
            strcpy(AllTables[numOfTables].table_column_name[column_counter], $1);
            strcpy(AllTables[numOfTables].table_column_type[column_counter], $2);
            column_counter++;
            $$ = 1;
    
            free($1);
            free($2);
            }
            ;
columns_def :column_def                     {$$=1;}
			|column_def COMMA columns_def   {$$=1+$3;}
			;
create_table: CREATE TABLE IDENTIFIER OPEN_PAR columns_def CLOSE_PAR SEMICOLON
            {
		    column_counter=0;
		    int local_counter=$5;
            for (int i = 0; i < numOfTables; i++) {
    	    if (strcmp(AllTables[i].table_name, $3) == 0) {
            printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La table '%s' existe deja.\n",line_numbers - 1,$3);
            YYABORT;
    	    }
		    }
        
            strcpy(AllTables[numOfTables].table_name, $3);
            AllTables[numOfTables].table_column_num = local_counter;
        
            printf("la table '%s' est cree avec succes!\n", $3);
            printf("nombre total des tables: %d\n", numOfTables + 1);
            printf("les colonnes dans cette table: %d\n", local_counter);
        
            for(int i = 0; i < local_counter; i++) {
            printf("  Colonne %d: %s %s\n",i+1,AllTables[numOfTables].table_column_name[i],AllTables[numOfTables].table_column_type[i]);
            }
        
            numOfTables++;
            local_counter = 0; 
        
            free($3);
            }
            ;

insert_command: INSERT INTO IDENTIFIER VALUES OPEN_PAR insert_values CLOSE_PAR SEMICOLON
                {
				int found = -1;
                for (int i = 0; i < numOfTables; i++) {
                if (strcmp(AllTables[i].table_name, $3) == 0) {
                found = i;
                break;
                }
                }

                if (found == -1) {
                printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La table '%s' n'existe pas.\n",line_numbers - 1,$3);
                YYABORT;
                }
                if (AllTables[found].table_column_num!=$6){
                    printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : le nombre des valeurs entrees est different du nombre des colonnes de la table '%s'\n",line_numbers - 1,$3);
                    YYABORT;
                }
                    printf("donnees inserees dans la table '%s' avec succes!\n", $3);
					printf("nombre des valeurs inserees : %d\n", $6);
					free($3);
                    YYACCEPT;
                }
                | INSERT INTO IDENTIFIER OPEN_PAR insert_columns CLOSE_PAR VALUES OPEN_PAR insert_values CLOSE_PAR SEMICOLON
				{
    			int table_index = -1;
    			for(int i = 0; i < numOfTables; i++) {
        		if(strcmp(AllTables[i].table_name, $3) == 0) {
            	table_index = i;
            	break;
        		}
    			}

    			if(table_index == -1) {
        		printf("ERREUR: Table '%s' n'existe pas\n", $3);
        		YYABORT;
    			}

                for(int i = 0; i < $5.number; i++) {
                for(int j = i + 1; j < $5.number; j++) {
                if(strcmp($5.names[i], $5.names[j]) == 0) {
                printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La colonne '%s' est repetee plusieurs fois parmi les colonnes.\n", 
                line_numbers - 1, $5.names[i]);
                YYABORT;
                }
                }
                }

    			for(int i = 0; i < $5.number; i++) {
        		int column_exists = 0;
        		for(int j = 0; j < AllTables[table_index].table_column_num; j++) {
            	if(strcmp($5.names[i], AllTables[table_index].table_column_name[j]) == 0) {
                column_exists = 1;
                break;
            	}
        		}
        
        		if(!column_exists) {
            	printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : Champ '%s' n'existe pas dans la table '%s'\n",line_numbers - 1,$5.names[i], AllTables[table_index].table_name);
            	YYABORT;
        		}
    			}		

                if($5.number != $9) {
        		printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : le nombre des colonnes et valeurs inserees ne correspond pas!\n",line_numbers - 1);
        		free($3);
        		YYABORT;
    			}

    			printf("donnees inserees dans la table '%s' avec succes!\n", $3);
    			printf("nombre des colonnes inserees : %d\n", $5.number);
    			printf("nombre des valeurs inserees : %d\n", $9);
    			free($3);
                YYACCEPT;
				}
				;

select_command: SELECT select_list FROM IDENTIFIER where_clause SEMICOLON
		        {
                int found = -1;
                for (int i = 0; i < numOfTables; i++) {
                if (strcmp(AllTables[i].table_name, $4) == 0) {
                found = i;
                break;
                }
                }

                if (found == -1) {
                printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La table '%s' n'existe pas.\n",line_numbers - 1,$4);
                YYABORT;
                }
                else{
                    if(strcmp($2,"*")==0){
                        printf("toutes les colonnes de la table %s sont selectionnees avec succes\n",$4);
                        printf("nombre total des colonnes selectionnees : %d\n",AllTables[found].table_column_num);
                    }else{
                     char *col = strdup($2);
                     char *token = strtok(col, ",");
            
            while(token != NULL) {
                while(*token == ' ') token++;
                
                int exists = 0;
                for(int i = 0; i < AllTables[found].table_column_num; i++) {
                    if(strcmp(token, AllTables[found].table_column_name[i]) == 0) {
                    exists = 1;
                    break;
                    }
                    }
                
                    if(!exists) {
                    printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : Colonne '%s' n'existe pas dans table '%s'\n", 
                    line_numbers - 1, token, $4);
                    free(col);
                    YYABORT;
                    }
                
                    token = strtok(NULL, ",");
                    }
                    free(col);
                        printf("les colonnes entrees de la table %s sont selectionnees avec succes\n",$4);
                        printf("nombre total des colonnes selectionnees : %d\n",vars_counter);
                    }
                    if($5==0) {
                        printf("clause WHERE : NON\n");
                    }
                    else{
                        printf("clause where : OUI\n");
                        printf("le nombre de conditions appliquees : %d\n",condition_counter);
                        printf("le nombre des operateurs logique utilises : %d\n",operator_counter);
                    }
                }

                }
	            ;

update_command:UPDATE IDENTIFIER SET newvalue where_clause SEMICOLON
		        {
                	int table_exists = 0;
					for(int i = 0; i < numOfTables; i++) {
    				if(strcmp(AllTables[i].table_name, $2) == 0) {
        			table_exists = 1;
        			break;
    				}
				}
					if(!table_exists) {
    				printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La table '%s' n\'existe pas.\n",line_numbers - 1,$2);
    				YYABORT;
				}
                if($5==1) {
                printf("mis a jour des elements de la table : %s\n ",$2);
                printf("clause WHERE : OUI\n");
                printf("le nombre des colonnes modifiees : %d\n",$4);
                if($4==1) {
                    printf("le champ modifie est %s\n",updated_columns[0]);
                    printf("le nombre de conditions appliquees : %d\n",condition_counter);
                    YYACCEPT;
                }
                else{
                printf("les champs modifiees sont : ( %s ",updated_columns[0]);
                for(int i=1;i<updated_counter - 1;i++){
                    printf(", %s",updated_columns[i]);
                }
                printf(" %s )\n",updated_columns[updated_counter - 1]);
                printf("le nombre des conditions appliquees : %d\n",condition_counter);
                YYACCEPT;
                }
                }
                else {
                int found=0;
                for (int i = 0; i < numOfTables; i++) {
                if (strcmp(AllTables[i].table_name, $2) == 0) {
                found = i;
                break;
                }
                }
                printf("mis a jour des elements de la table : %s\n ",$2);
                printf("clause where : NON\n");
                printf("le nombre des colonnes modifiees : %d\n",$4);
                if($4==1) {
                    printf("le champ modifie est %s\n",updated_columns[0]);
                    printf("le nombre de conditions appliquees : %d\n",condition_counter);
                    YYACCEPT;
                }
                else{
                printf("les champs modifiees sont : ( %s ",updated_columns[0]);
                for(int i=1;i<updated_counter - 1;i++){
                    printf(", %s",updated_columns[i]);
                }
                printf(" %s )\n",updated_columns[updated_counter - 1]);
                YYACCEPT;
                }
                }
                }
	            ;

delete_command:DELETE FROM IDENTIFIER where_clause SEMICOLON
		{
            int table_exists = 0;
			for(int i = 0; i < numOfTables; i++) {
    		if(strcmp(AllTables[i].table_name, $3) == 0) {
        	table_exists = 1;
        	break;
    		}
			}
			if(!table_exists) {
    		printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La table '%s' n\'existe pas.\n",line_numbers - 1,$3);
    		YYABORT;
            }
            if($4==0){
            printf("clause WHERE : NON\n");
            printf("toutes les colonnes de la table '%s' sont videe avec succes!\n",$3);
            YYACCEPT;
            }
            else     {printf("clasue WHERE : OUI\n");
            printf("le nombre des conditions appliquees : %d\n",condition_counter);
            printf("donnees supprimees de la table '%s' avec succes!\n",$3);
            YYACCEPT;
            }
            }
 	        ;

drop_command:DROP TABLE IDENTIFIER SEMICOLON
   {
        int found = -1;
        for (int i = 0; i < numOfTables; i++) {
            if (strcmp(AllTables[i].table_name, $3) == 0) {
                found = i;
                break;
            }
        }

        if (found == -1) {
            printf("ERREUR SEMANTIQUE DANS LA LIGNE %d : La table '%s' n'existe pas.\n",line_numbers - 1,$3);
            YYABORT;
        } else {
            for (int j = found; j < numOfTables - 1; j++) {
                AllTables[j] = AllTables[j + 1];
            }
            numOfTables--;
            printf("La table '%s' a ete supprimee avec succes.\n", $3);
            printf("le nombre de tables restantes est %d\n",numOfTables);
            YYACCEPT;
        }
    }
;
%%
extern int yylex_destroy(void);

int yyerror(const char *msg) {
    printf("ERREUR DE SYNTAX DANS LA LIGNE %d\n",line_numbers - 1);
    parse_error = 1;
    return 0;
}

int main() {
    printf("Bienvenue au interpreteur SQL.\n");
    printf("Entrez des requetes terminants par ';'.\n");
    printf("Entrez Ctrl+C ou EXIT pour quitter.\n\n");

    char command[MAX_CMD_LEN] = {0};
    char line[256];

    numOfTables = 0;
    column_counter = 0;
    parse_error = 0;
    condition_counter = 0;
    operator_counter = 0;

    while (1) {
        printf("%d> ",line_numbers);
        fflush(stdout);

        if (!fgets(line, sizeof(line), stdin)) {
            printf("\nAu revoir!\n");
            break;
        }
        line_numbers++;

        if (strncasecmp(line, "exit", 4) == 0 || strncasecmp(line, "quit", 4) == 0) {
            printf("Au revoir!\n");
            break;
        }

        if (strlen(command) + strlen(line) >= MAX_CMD_LEN - 1) {
            fprintf(stderr, "ERREUR : requete trop longue.\n");
            command[0] = '\0';
            column_counter = 0;
            continue;
        }

        strcat(command, line);

        if (strchr(command, ';')) {
            parse_error = 0;
            column_counter = 0;
            condition_counter = 0;
            updated_counter = 0;
            vars_counter = 0;
            operator_counter = 0;

            YY_BUFFER_STATE buffer = yy_scan_string(command);
            yyparse();
            yy_delete_buffer(buffer);
            yylex_destroy();
            yyrestart(stdin);

            command[0] = '\0';

        }
    }

    return 0;
}


