#include "lex.yy.c"

int main (){
	int parseint;
  	while(parseint = yyparse()){
		if(parseint < 0) 
			exit(0);
  	}
  	return 0;
}