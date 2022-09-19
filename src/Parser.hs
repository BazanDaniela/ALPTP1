module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip" , "do"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ,"?"
                        ,":"
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = try (do b <- boolatom
                 reservedOp lis "?"
                 i1 <- intexp
                 reservedOp lis ":"
                 ECond b i1 <$> intexp)
         <|> expr

expr :: Parser (Exp Int)
expr = chainl1 term plusMin
        where plusMin = try (do {reservedOp lis "+";
                                 return Plus}) 
                           <|> do reservedOp lis "-"
                                  return Minus

term :: Parser (Exp Int)
term = chainl1 factor mulDiv
        where mulDiv = try (do {reservedOp lis "*";
                                 return Times}) 
                          <|> do reservedOp lis "/"
                                 return Div

factor :: Parser (Exp Int)
factor = do reservedOp lis "-"
            UMinus <$> factor
           <|> do Const . fromIntegral <$> natural lis
           <|> do parens lis intexp
           <|> do Var <$> identifier lis

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 bool1 orop
            where orop = do { reservedOp lis "||"; return (Or)}


bool1 :: Parser (Exp Bool)
bool1 = chainl1 bool2 andop
          where andop = do { reservedOp lis "&&"; return (And)}

bool2 :: Parser (Exp Bool)
bool2 = try ( do i <- intexp
                 do reservedOp lis "=="
                    Eq i <$> intexp
                   <|> do reservedOp lis "!="
                          NEq i <$> intexp
                   <|> do reservedOp lis "<"
                          Lt i <$> intexp
                   <|> do reservedOp lis ">"
                          Gt i <$> intexp)
        <|> boolatom
boolatom :: Parser (Exp Bool)
boolatom = do reserved lis "true"
              return BTrue
              <|> do reserved lis "false"
                     return BFalse
              <|> do reservedOp lis "!"
                     Not <$> boolatom
              <|> parens lis boolexp
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm2 sequence
        where sequence = do {reservedOp lis ";"; return (Seq)}

comm2 :: Parser Comm
comm2 = do reserved lis "if"
           b <- boolexp
           reservedOp lis "{"
           ct <- comm
           reservedOp lis "}"
           reserved lis "else"
           reservedOp lis "{"
           cf <- comm
           reservedOp lis "}"
           return (IfThenElse b ct cf)
          <|> do reserved lis "while"
                 b <- boolexp
                 reservedOp lis "{"
                 c <- comm
                 reservedOp lis "}"
                 return (While b c)
          <|> do reserved lis "skip"
                 return Skip
          <|> do v <- identifier lis
                 reservedOp lis "="
                 Let v <$> intexp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
