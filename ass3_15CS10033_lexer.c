#include "lex.yy.c"
extern char* yytext;

int main() {
	int token;
	while(token = yylex()) {
		if(token >= 258 && token <= 280) {
			printf("<KEYWORD, %s>\n", yytext);	
		}
		if(token >= 281 && token <= 284) {
			printf("<CONSTANT, %s>\n", yytext);
		}
		if(token == 286) {
			printf("<IDENTIFIER, %s>\n", yytext);
		}
		if(token == 285) {
			printf("<STRING_LITERAL, %s>\n", yytext);
		}
		if((token >= 287 && token <= 308) || token == '[' || token == ']' || token == '(' || token == ')' || token == '{' || token == '}' || token == '.' || token == '&' || token == '*' || token == '+' || token == '-' || token == '~' || token == '!' || token == '/' || token == '%' || token == '<' || token == '>' || token == '^' || token == '|' || token == '?' || token == ':' || token == ';' || token == '=' || token == ',' || token == '#' ) {
			printf("<PUNCTUATOR, %s>\n", yytext);
		}
	}
	return 0;
}
