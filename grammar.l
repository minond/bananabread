expr = prefix-op primary
     | primary infix-op primary
     | primary postfix-op
     ;

primary = "(" expr ")"
        | lambda
        | cond
        | let
        | begin
        | literal
        ;

lambda = "func" "(" lambda-args ")" "=" expr
       ;

lambda-args = [ lambda-arg "," { lambda-args } ]
            ;

cond = "if" expr "then" expr "else" expr
     ;

let = "let" { id1 "=" expr } "in" expr
    ;

begin = "begin" { expr } "end"
      ;

literal = id1
        | id2
        | number
        | string
        | symbol
        ;

id1 = id1-head , id1-tail
    ;

id1-head = id1-tail - digit
         ;

id1-tail = [ letter ]
         | [ digit ]
         | "_"
         | { id1-tail }
         ;

id2 = char , id2-tail
    ;

id2-tail = ? all visible chars minus whitespace ? - id1-tail - reserved-token
         ;

number = digit , { digit }
       ;

symbol = "'" , { chars - "(" | ")" | "{" | "}" | "[" | "]" }
       ;

string = "%{" { chars - "}" } "}"
       ;

char = ? any visible character ?
     ;

chars = ? all visible characters ?
      ;

letter = "a" | ... | "z"
       | "A" | ... | "Z"
       ;

digit = "0" | ... | "9"
      ;

reserved-token = "," | "." | "(" | ")" | "{" | "}" | "[" | "]"
               ;
