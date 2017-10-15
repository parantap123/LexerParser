%{
    #include <stdio.h>
    #include "ass6_12CS30006_translator.h"
    #include "y.tab.h"
    int yyerror(char *);
    extern int yylex(void);
%}

%token UNSIGNED BREAK RETURN VOID CASE FLOAT SHORT CHAR FOR SIGNED WHILE GOTO BOOL CONTINUE IF DEFAULT DO INT SWITCH DOUBLE LONG ELSE MATRIX 
%token INT_CONST FLOAT_CONST ZERO_CONST CHAR_CONST 
%token STRING_LITERAL 
%token IDENTIFIER 
%token PTR INC_OP DEC_OP SHL SHR LEQ GEQ EQEQ NEQ LOG_OR LOG_AND MULASG DIVASG MODASG ADDASG SUBASG SHLASG SHRASG ANDASG XORASG ORASG UNK
%token WS
%start translation_unit

%%

primary_expression:
  IDENTIFIER {
    if(DEBUG) 
        printf("primary_expression --> identifier\n");
    $$ = symbTableStack.top()->lookup($1->name);
    $$ = new symbol(*$$);
}
| INT_CONST {
    if(DEBUG) 
        printf("primary_expression --> integer_constant\n");
}
| ZERO_CONST 
| FLOAT_CONST {
    if(DEBUG) 
        printf("primary_expression --> float_constant\n");
}
| CHAR_CONST {
    if(DEBUG) 
        printf("primary_expression --> character_constant\n");
}
| STRING_LITERAL {
    if(DEBUG) 
        printf("primary_expression")
}
| '(' expression ')' {
    if(DEBUG) 
        printf("primary_expression --> ( expression )");
    $$ = $2;
}
;

postfix_expression:
  primary_expression {
    if(DEBUG)
        printf("postfix_expression --> primary_expression\n");
}
| IDENTIFIER '[' expression ']' '[' expression ']' {
    if(DEBUG)
        printf("postfix_expression --> [ expression ]\n");
}
| postfix_expression '(' argument_expression_list ')' {
    if(DEBUG)
      printf("postfix_expression --> ( argument_expression_list_opt )\n");
    $3->call($1);
    $$ = $1;
}
| postfix_expression '(' ')' {
    if(DEBUG)
        printf("postfix_expression --> postfix_expression ()\n");
}
| postfix_expression '.' IDENTIFIER {
    if(DEBUG)
        printf("postfix_expression . identifier\n");
}
| postfix_expression PTR IDENTIFIER {
    if(DEBUG)
        printf("postfix_expression --> postfix_expression -> identifier\n");
}
| postfix_expression INC_OP {
    if(DEBUG)
        printf("postfix_expression --> postfix_expression ++\n");
    $$ = gentemp(($1->type).typeName);
    emit($$->name, _ADD, $1->name, "1");
}
| postfix_expression DEC_OP {
    if(DEBUG)
        printf("postfix_expression --> postfix_expression --\n");
    $$ = gentemp(($1->type).typeName);
    emit($$->name, _MINUS, $1->name, "1");
}
| postfix_expression UNK {printf("postfix_expression --> postfix_expression .'\n");}
;

argument_expression_list:
  assignment_expression {
    if(DEBUG)
        printf("argument_expression_list --> argument_expression\n");
    $$ = new argList();
    $$->args.push_back($1);
}
| argument_expression_list ',' assignment_expression {
    if(DEBUG)
        printf("assignment_expression_list --> assignment_expression_list , assignment_expression\n");
    $1->args.push_back($3);
    $$ = $1;
}
;

unary_expression:
  postfix_expression {
    if(DEBUG)
        printf("unary_expression --> postfix_expression\n");
}
| INC_OP unary_expression {
    if(DEBUG)
        printf("unary_expression --> ++ unary_expression\n");
    emit($2->name, _ADD, $2->name, "1");
    $$ = $2;      
}
| DEC_OP unary_expression {
    if(DEBUG)
        printf("unary_expression --> -- unary_expression\n");
    emit($2->name, _MINUS, $2->name, "1");
    $$ = $2;        
}
| unary_operator cast_expression {
    if(DEBUG)
        printf("unary_expression --> unary_operator cast_expression\n");
    switch($3) {
        case '&':
            typeCL ty = ($2->type).getBaseType();
            ty = typeCL(_PTR, 0, ty);
            $$ = gentemp(ty);
            emit($$->name, _ADDRESS, $2->name);
            break;
        case '*':
            
    }
}
;

unary_operator :
  '&' {
    if(DEBUG)
        printf("unary_operator --> &\n");
    $$ = '&';
}
| '*' {
    if(DEBUG)
        printf("unary_operator --> *\n");
    $$ = '*'; 
}
| '+' {
    if(DEBUG)
        printf("unary_operator --> +\n");
    $$ = '+';
}
| '-' {
    if(DEBUG)
        printf("unary_operator --> -\n");
    $$ = '-';
}
;

cast_expression :
  unary_expression {
    if(DEBUG)
        printf("cast_expression --> unary_expression\n");
    $$ = $1;
}
;

multiplicative_expression :
  cast_expression {printf("multiplicative_expression --> cast_expression\n");}
| multiplicative_expression '*' cast_expression {printf("multiplicative_expression --> multiplicative_expression * cast-expression\n");}
| multiplicative_expression '/' cast_expression {printf("multiplicative_expression --> multiplicative_expression / cast_expression\n");}
| multiplicative_expression '%' cast_expression {printf("multiplicative_expression --> multiplicative_expression %% cast_expression\n");}
;

additive_expression :
  multiplicative_expression {printf("additive_expression --> multiplicative_expression\n");}
| additive_expression '+' multiplicative_expression {printf("additive_expression --> additive_expression + multiplicative_expression\n");}
| additive_expression '-' multiplicative_expression {printf("additive_expression --> additive_expression - multiplicative_expression\n");}
;

shift_expression :
  additive_expression {printf("shift_expression --> additive_expression\n");}
| shift_expression SHL additive_expression {printf("shift_expression --> shift_expression << additive_expression\n");}
| shift_expression SHR additive_expression {printf("shift_expression --> shift_expression >> additive_expression\n");}
;

relational_expression :
  shift_expression {printf("relational_expression --> shift_expression\n");}
| relational_expression '<' shift_expression {printf("relational_expression --> relational_expression < shift_expression\n");}
| relational_expression '>' shift_expression {printf("relational_expression --> relational_expression > shift_expression\n");}
| relational_expression LEQ shift_expression {printf("relational_expression --> relational_expression <= shift_expression\n");}
| relational_expression GEQ shift_expression {printf("relational_expression --> relational_expression >= shift_expression\n");}
;

equality_expression :
  relational_expression {printf("equality_expression --> relational_expression\n");}
| equality_expression EQEQ relational_expression {printf("equality_expression == relational_expression\n");}
| equality_expression NEQ relational_expression {printf("equality_expression != relational_expression\n");}
;

AND_expression :
  equality_expression {printf("AND_expression --> equality_expression\n");}
| AND_expression '&' equality_expression {printf("AND_expression --> AND_expression & equality_expression\n");}
;

exclusive_OR_expression :
  AND_expression {printf("exclusive_OR_expression --> AND_expression\n");}
| exclusive_OR_expression '^' AND_expression {printf("exclusive_OR_expression ^ AND_expression\n");}
;

inclusive_OR_expression :
  exclusive_OR_expression {printf("inclusive_OR_expression --> exclusive_OR_expression\n");}
| inclusive_OR_expression '|' exclusive_OR_expression {printf("inclusive_OR_expression --> inclusive_OR_expression | exclusive_OR_expression\n");}
;

logical_AND_expression :
  inclusive_OR_expression {printf("logical_AND_expression --> inclusive_OR_expression\n");}
| logical_AND_expression LOG_AND inclusive_OR_expression {printf("logical_AND_expression --> logical_AND_expression && inclusive_OR_expression\n");}
;

logical_OR_expression :
  logical_AND_expression {printf("logical_OR_expression --> logical_AND_expression\n");}
| logical_OR_expression  LOG_OR logical_AND_expression {printf("logical_OR_expression --> logical_OR_expression || logical_AND_expression\n");}
;

conditional_expression :
  logical_OR_expression {printf("conditional_expression --> logical_OR_expression\n");}
| logical_OR_expression  '?' expression ':' conditional_expression {printf("conditional_expression --> logical_OR_expression ? expression : conditional_expression\n");}
;

assignment_expression_opt :
  %empty {printf("assignment_expression_opt --> ε\n");}
| assignment_expression {printf("assignment_expression_opt --> assignment_expression\n");}
;

assignment_expression :
  conditional_expression {printf("assignment_expression --> conditional_expression\n");}
| unary_expression assignment_operator assignment_expression {printf("assignment_expression --> unary_expression assignment_operator assignment_expression\n");}
;

assignment_operator :
  '=' {printf("assignment_operator --> =\n");}
| MULASG {printf("assignment_operator --> *=\n");}
| DIVASG {printf("assignment_operator --> /=\n");}
| MODASG {printf("assignment_operator --> %%=\n");}
| ADDASG {printf("assignment_operator --> +=\n");}
| SUBASG {printf("assignment_operator --> -=\n");}
| SHLASG {printf("assignment_operator --> <<=\n");}
| SHRASG {printf("assignment_operator --> >>=\n");}
| ANDASG {printf("assignment_operator --> &=\n");}
| XORASG {printf("assignment_operator --> ^=\n");}
| ORASG {printf("assignment_operator --> |=\n");}
;

expression :
  assignment_expression {printf("expression --> assignment_expression\n");}
| expression ',' assignment_expression {printf("expression --> expression , assignment_expression\n");}
;

constant_expression :
  conditional_expression {printf("constant_expression --> conditional_expression\n");}
;

declaration :
  declaration_specifiers init_declarator_list_opt ';' {printf("declaration --> declaration_specifiers init_declarator_list_opt ;\n");}
;

declaration_specifiers_opt :
  %empty {printf("declaration_specifiers_opt --> ε\n");}
| declaration_specifiers {printf("declaration_specifiers_opt --> declaration_specifiers\n");}
;

declaration_specifiers :
  type_specifier declaration_specifiers_opt {printf("declaration_specifiers --> type_specifier declaration_specifiers_opt\n");}
;

init_declarator_list_opt :
  %empty {printf("init_declarator_list_opt --> ε\n");}
| init_declarator_list {printf("init_declarator_list_opt --> init_declarator_list\n");}
;

init_declarator_list :
  init_declarator {printf("init_declarator_list --> init_declarator\n");}
| init_declarator_list ',' init_declarator {printf("init_declarator_list --> init_declarator_list , init_declarator\n");}
;

init_declarator :
  declarator {printf("init_declarator --> declarator\n");}
| declarator '=' initializer {printf("declarator = initializer\n");}
;

type_specifier :
  VOID {printf("type_specifier --> void\n");}
| CHAR {printf("type_specifier --> char\n");}
| SHORT {printf("type_specifier --> short\n");}
| INT {printf("type_specifier --> int\n");}
| LONG {printf("type_specifier --> long\n");}
| FLOAT {printf("type_specifier --> float\n");}
| DOUBLE {printf("type_specifier --> double\n");}
| MATRIX {printf("type_specifier --> Matrix\n");}
| SIGNED {printf("type_specifier --> signed\n");}
| UNSIGNED {printf("type_specifier --> unsigned\n");}
| BOOL {printf("type_specifier --> Bool\n");}
;

declarator :
  pointer_opt direct_declarator {printf("declarator --> pointer_opt direct_declarator\n");}

direct_declarator :
  IDENTIFIER {printf("direct_declarator --> identifier\n");}
| '(' declarator ')' {printf("direct_declarator --> ( declarator )\n");}
| direct_declarator '[' assignment_expression_opt ']' {printf("direct_declarator --> direct_declarator [ assignment_expression_opt ]\n");}
| direct_declarator '(' parameter_type_list ')' {printf("direct_declarator --> direct_declarator ( parameter_type_list )\n");}
| direct_declarator '(' identifier_list_opt ')' {printf("direct_declarator --> direct_declarator ( identifier_list_opt )\n");}
;

pointer_opt :
  %empty {printf("pointer_opt --> ε\n");}
| pointer {printf("pointer_opt --> pointer\n");}
;

pointer :
  '*' pointer_opt {printf("* pointer_opt\n");}
;

parameter_type_list :
  parameter_list {printf("parameter_type_list --> parameter_list\n");}
;

parameter_list :
  parameter_declaration {printf("parameter_list --> parameter_declaration\n");}
| parameter_list ',' parameter_declaration {printf("parameter_list --> parameter_list , parameter_declaration\n");}
;

parameter_declaration :
  declaration_specifiers declarator {printf("parameter_declaration --> declaration_specifiers declarator\n");}
| declaration_specifiers {printf("parameter_declaration --> declaration_specifiers\n");}
;

identifier_list_opt :
  %empty {printf("identifier_list_opt --> ε\n");}
| identifier_list {printf("identifier_list_opt --> identifier_list\n");}
;

identifier_list :
  IDENTIFIER {printf("identifier_list --> identifier\n");}
| identifier_list ',' IDENTIFIER {printf("identifier_list --> identifier_list , identifier\n");}
;

initializer :
  assignment_expression {printf("initializer --> assignment_expression\n");}
| '{' initializer_row_list '}' {printf("initializer --> { initializer_row_list }\n");}
;

initializer_row_list :
  initializer_row {printf("initializer_row_list --> initializer_row\n");}
| initializer_row_list ';' initializer_row {printf("initializer_row_list --> initializer_row_list ; initializer_row\n");}
;

initializer_row :
  designation_opt initializer {printf("initializer_row --> designation_opt initializer\n");}
| initializer_row ',' designation_opt initializer {printf("initializer_row , designation_opt initializer\n");}
;

designation_opt :
  %empty {printf("designation_opt --> ε\n");}
| designation {printf("designation_opt --> designation\n");}
;

designation :
  designator_list '=' {printf("designation --> designator_list =\n");}
;

designator_list :
  designator {printf("designator_list --> designator\n");}
| designator_list designator {printf("designator_list --> designator_list designator\n");}
;

designator :
  '[' constant_expression ']' {printf("[ constant_expression ]\n");}
| '.' IDENTIFIER {printf("designator --> . identifier\n");}
;

statement :
  labeled_statement {printf("statement --> labeled_statement\n");}
| compound_statement {printf("statement --> compound_statement\n");}
| expression_statement {printf("statement --> expression_statement\n");}
| selection_statement {printf("statement --> selection_statement\n");}
| iteration_statement {printf("statement --> iteration_statement\n");}
| jump_statement {printf("statement --> jump_statement\n");}
;

labeled_statement :
  IDENTIFIER ':' statement {printf("labeled_statement --> identifier : statement\n");}
| CASE constant_expression ':' statement {printf("labeled_statement --> case constant_expression : statement\n");}
| DEFAULT ':' statement {printf("labeled_statement --> default : statement\n");}
;

compound_statement :
  '{' block_item_list_opt '}' {printf("compound_statement --> { block_item_list_opt }\n");}
;

block_item_list_opt :
  %empty {printf("block_item_list_opt --> ε\n");}
| block_item_list {printf("block_item_list_opt --> block_item_list\n");}
;

block_item_list :
  block_item {printf("block_item_list --> block_item\n");}
| block_item_list block_item {printf("block_item_list --> block_item_list block_item\n");}
;

block_item :
  declaration {printf("block_item --> declaration\n");}
| statement {printf("block_item --> statement\n");}
;

expression_statement :
  expression_opt ';' {printf("expression_statement --> expression_opt ;\n");}
;

expression_opt :
  %empty {printf("expression_opt --> ε\n");}
| expression {printf("expression_opt --> expression\n");}
;

selection_statement :
  IF '(' expression ')' statement {printf("selection_statement --> if ( expression ) statement\n");}
| IF '(' expression ')' statement ELSE statement {printf("selection_statement --> if ( expression ) statement else statement\n");}
| SWITCH '(' expression ')' statement {printf("selection_statement --> switch ( expression ) statement\n");}
;

iteration_statement :
  WHILE '(' expression ')' statement {printf("iteration_statement --> while ( expression ) statement\n");}
| DO statement WHILE '(' expression ')' ';' {printf("iteration_statement --> do statement while ( expression ) ;\n");}
| FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement {printf("iteration_statement --> for ( expression_opt ; expression_opt ; expression_opt ) statement\n");}
| FOR '(' declaration expression_opt ';' expression_opt ')' statement {printf("iteration_statement --> for ( declaration expression_opt ; expression_opt ) statement\n");}
;

jump_statement :
  GOTO IDENTIFIER ';' {printf("jump_statement --> goto identifier ;\n");}
| CONTINUE ';' {printf("jump_statement --> continue ;\n");}
| BREAK ';' {printf("jump_statement --> break ;\n");}
| RETURN expression_opt ';' {printf("return expression_opt\n");}
;

translation_unit :
  external_declaration {printf("translation_unit --> external_declaration\n");}
| translation_unit external_declaration {printf("translation_unit --> translation_unit external_declaration\n");}
;

external_declaration :
  function_definition {printf("external_declaration --> function_definition\n");}
| declaration {printf("external_declaration --> declaration\n");}
;

function_definition :
  declaration_specifiers declarator declaration_list_opt compound_statement {printf("function_definition --> declaration_specifiers declarator declaration_list_opt compound_statement\n");}
;

declaration_list_opt :
  %empty {printf("declaration_list_opt --> ε\n");}
| declaration_list {printf("declaration_list_opt --> declaration_list\n");}
;

declaration_list :
  declaration {printf("declaration_list --> declaration\n");}
| declaration_list declaration {printf("declaration_list --> declaration_list declaration\n");}
;

%%

int yyerror(char *s){
    printf("Parser Error : %s\n",s);
    exit(0);
}

