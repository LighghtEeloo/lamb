let var  = ['a'-'z' 'A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let num = ['0' - '9']+

let ws = [' ' '\t' '\r' '\n' '\011' '\012'] 

rule initial = parse
  | ws+           { initial lexbuf }

  | var  as v     { Parse.VAR  v }
  | '\\'          { Parse.LAMBDA }
  | '.'           { Parse.DOT    }
  | '('           { Parse.LPAR   }
  | ')'           { Parse.RPAR   }
  | ';'           { Parse.SEMI   }
  | eof           { Parse.EOF    }
