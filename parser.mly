%{
open Syntax
let addtyp x = (x, Type.gentyp ())
%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token AST
%token SLASH
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token ITOF
%token FSQR
%token SQRT
%token FABS
%token FLESS
%token FNEG
%token FISZERO
%token FISNEG
%token FISPOS
%token FSGNJ
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF
%token NEW_LINE

%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST SLASH AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

%type <Syntax.t> toplevel
%start toplevel

%%

simple_exp:
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var($1) }
| simple_exp DOT LPAREN exp RPAREN
    { Get($1, $4) }

toplevel:
| LET IDENT EQUAL exp toplevel
    { GlobalLet(addtyp $2, $4, $5) }
| LET IDENT EQUAL exp
    { GlobalLet(addtyp $2, $4, Unit) }
| exp
    { $1 }

exp:
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f)
    | e -> Neg(e) }
| exp PLUS exp
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp AST INT
    { Mul($1, $3) }
| exp SLASH INT
    { Div($1, $3) }
| exp EQUAL exp
    { Eq($1, $3) }
| exp LESS_GREATER exp
    { Not(Eq($1, $3)) }
| exp LESS exp
    { Not(LE($3, $1)) }
| exp GREATER exp
    { Not(LE($1, $3)) }
| exp LESS_EQUAL exp
    { LE($1, $3) }
| exp GREATER_EQUAL exp
    { LE($3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($2, $4, $6) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2) }
| exp PLUS_DOT exp
    { FAdd($1, $3) }
| exp MINUS_DOT exp
    { FSub($1, $3) }
| exp AST_DOT exp
    { FMul($1, $3) }
| exp SLASH_DOT exp
    { FDiv($1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET BOOL EQUAL exp IN exp
    { $6 }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5) }
| simple_exp actual_args
    %prec prec_app
    { App($1, $2) }
| elems
    %prec prec_tuple
    { Tuple($1) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { LetTuple($3, $6, $8) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7) }
| exp SEMICOLON exp
    { Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) }
| exp SEMICOLON
    { Let((Id.gentmp Type.Unit, Type.Unit), $1, Unit) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3) }
| ITOF simple_exp
    %prec prec_app
    { Itof($2) }
| FSQR simple_exp
    %prec prec_app
    { FSqr($2) }
| SQRT simple_exp
    %prec prec_app
    { Sqrt($2) }
| FABS simple_exp
    %prec prec_app
    { FAbs($2) }
| FLESS simple_exp simple_exp
    %prec prec_app
    { FLess($2, $3) }
| FNEG simple_exp
    %prec prec_app
    { FNeg($2) }
| FISZERO simple_exp
    %prec prec_app
    { FIsZero($2) }
| FISPOS simple_exp
    %prec prec_app
    { FIsPos($2) }
| FISNEG simple_exp
    %prec prec_app
    { FIsNeg($2) }
| FSGNJ simple_exp simple_exp
    %prec prec_app
    { FSgnj($2, $3) }
| error {
    failwith
        (
            let start_pos = Parsing.symbol_start_pos() in
            let end_pos = Parsing.symbol_end_pos() in
            Printf.sprintf "parse error near characters l%d %d - l%d %d"
            start_pos.pos_lnum
            (start_pos.pos_cnum - start_pos.pos_bol)
            end_pos.pos_lnum
            (end_pos.pos_cnum - end_pos.pos_bol)
           )
        }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
