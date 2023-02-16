{
module HaskellLexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

    ($alpha)+($digit)*      {\s -> TokenVar s}
    $digit+                 {\s -> TokenInt (read s)}
    "let"                   {\s -> TokenLet}
    "in"                    {\s -> TokenIn}
    "case"                  {\s -> TokenCase}
    "of"                    {\s -> TokenOf}
    "="                     {\s -> TokenEq}
    ($alpha)+               {\s -> TokenFun s}
    $white+                 ; 
    .                       ;

{
data Token 
    = TokenVar String
    | TokenInt Int
    | TokenLet
    | TokenIn
    | TokenCase
    | TokenOf
    | TokenEq
    | TokenFun String
    deriving (Eq,Show)
}