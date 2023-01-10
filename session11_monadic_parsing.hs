import Parsing


data Onion = Core Int | Layer  Onion deriving Show
theonion :: Parser Onion
theonion =
    do char 'L'
       l <- theonion
       return (Layer l)
    <|>
    do x <- int
       return (Core x)

data Grammar = Multi Char Grammar Char | Empty deriving Show

theGrammar :: Parser Grammar
theGrammar =
    do
        x <- char 'a'
        grammar <- theGrammar
        y <- char 'b'
        return (Multi x grammar y)
    <|>
    do return Empty


--- This must be converter to a language that does not need lookahead
data Rexp = A | B | Union Rexp Rexp | Concat Rexp Rexp | Kstar Rexp deriving Show

--- exp := Term 'O' exp | term U exp       <- concat and union. Concat takes precedence
--- term := '(' expr ') '*' | factor
--- factor := 'a' | 'b'
--- We then create rules for these:
expr = do x <- term
          char 'O'
          y <- expr
          return (Concat x y)
       <|>
       do x <- term
          char 'U'
          y <- expr
          return (Union x y)
       <|> term

term = do char '('
          x <- expr
          char ')'
          char '*'
          return (Kstar x)
       <|> factor

factor = do char 'a'
            return A
         <|>
         do char 'b'
            return B