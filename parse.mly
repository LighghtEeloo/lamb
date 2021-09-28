%{
module Expr = Ast.Expr
%}

%token <string> VAR
%token LAMBDA
%token DOT
%token LPAR
%token RPAR
%token SEMI
%token EOF

%start <Ast.Top.t list> main

%%

main :
| cmds=cmds EOF                 { cmds   }

cmds :
| cmd SEMI                      { [ $1 ] }
| cmd SEMI cmds                 { $1::$3 }

cmd :
| expr                          { Ast.Top.create $1 }

expr :
| e=expr ea=atomic              { Expr.App (e, ea)  }
| e=atomic                      { e }

atomic :
| v=var                         { Expr.Var v        }
| LAMBDA v=var DOT e=atomic     { Expr.Lam (v, e)   }
| LPAR e=expr RPAR              { e }

var :
| VAR                           { Ast.Var.create $1 }
