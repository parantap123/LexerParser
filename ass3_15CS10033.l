%{
	#include "ass3_15CS10033.tab.c"
%}

id_nonDigit 	[_a-zA-Z]
digit 			[0-9]	
id 				{id_nonDigit}+({digit}|{id_nonDigit})*
zero_const		[0]
int_const 		[1-9][0-9]*
sign 			[+-]
digit_seq		{digit}+
frac_const		({digit_seq}?"."{digit_seq})|({digit_seq}".")
exponent_part	[eE]{sign}?{digit_seq}
float_const		({frac_const}{exponent_part}?)|({digit_seq}{exponent_part})
esc_seq			 ("\\'"|"\\\""|"\\?"|"\\\\"|"\\a"|"\\b"|"\\f"|"\\n"|"\\r"|"\\t"|"\\v")
c_char  		[^'\\\n]|{esc_seq}
c_char_seq		{c_char}+
char_const		\'{c_char_seq}\'
s_char_seq		{c_char}+
s_literal		\"{c_char_seq}\"
whitespace		[ \t\n\r\v\f]

%option noyywrap
%x comment
%%



{whitespace}	{}
"unsigned"		{return UNSIGNED;}
"break"			{return BREAK;}
"return"		{return RETURN;}
"void"			{return VOID;}
"case"			{return CASE;}
"float"			{return FLOAT;}
"short"			{return SHORT;}
"char"			{return CHAR;}
"for"			{return FOR;}
"signed"		{return SIGNED;}
"while"			{return WHILE;}
"goto"			{return GOTO;}
"Bool"			{return BOOL;}
"continue"		{return CONTINUE;}
"if"			{return IF;}
"default"		{return DEFAULT;}
"do"			{return DO;}
"int"			{return INT;}
"switch"		{return SWITCH;}
"double"		{return DOUBLE;}
"long"			{return LONG;}
"else"			{return ELSE;}
"Matrix"		{return MATRIX;}

{s_literal}		{return STRING_LITERAL;}
{int_const} 	{return INT_CONST;}
{float_const}	{return FLOAT_CONST;}
{char_const}	{return CHAR_CONST;}
{zero_const}    {return INT_CONST;}
{id}			{return IDENTIFIER;}

"//".*			{}

"/*"         BEGIN(comment);

<comment>[^*\n]*        {}
<comment>"*"+[^*/\n]*   {}
<comment>\n             {}
<comment>"*"+"/"        BEGIN(INITIAL);

"["				{return '[';}		
"]"				{return ']';}
"("				{return '(';}
")"				{return ')';}
"{"				{return '{';}
"}"				{return '}';}
"."				{return '.';}
"->"			{return PTR;}
"++"			{return INC_OP;}
"--"			{return DEC_OP;}
"&"				{return '&';}
"*"				{return '*';}
"+"				{return '+';}
"-"				{return '-';}
"~"				{return '~';}
"!"				{return '!';}
"/"				{return '/';}
"%"				{return '%';}
"<<"			{return SHL;}
">>"			{return SHR;}
"<"				{return '<';}
">"				{return '>';}
"<="			{return LEQ;}
">="			{return GEQ;}
"=="			{return EQEQ;}
"!="			{return NEQ;}
"^"				{return '^';}
"|"				{return '|';}
"&&"			{return LOG_AND;}
"||"			{return LOG_OR;}
"?"				{return '?';}
":"				{return ':';}
";"				{return ';';}
"="				{return '=';}
"*="			{return MULASG;}
"/="			{return DIVASG;}
"%="			{return MODASG;}
"+="			{return ADDASG;}
"-="			{return SUBASG;}
"<<="			{return SHLASG;}
">>="			{return SHRASG;}
"&="			{return ANDASG;}
"^="			{return XORASG;}
"|="			{return ORASG;}
","				{return ',';}
"#"				{return '#';}
".'"			{return UNK;}

%%
