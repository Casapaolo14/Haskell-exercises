{
module RPNLexer where
}

%wrapper "basic"

$digit = [0-9]

tokens :-

    $digit+             {\s -> TokenInt (read s)}
    "/"                 {\s -> TokenDivide}
    "*"                 {\s -> TokenTimes}
    "-"                 {\s -> TokenMinus}
    "+"                 {\s -> TokenPlus}
    $white+             ; 
    .                   ;

{
data Token 
    = TokenInt Int
    | TokenDivide
    | TokenTimes
    | TokenMinus
    | TokenPlus
    deriving (Eq,Show)
}