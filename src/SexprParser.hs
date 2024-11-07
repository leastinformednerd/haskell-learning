module SexprParser (parseSexpr) where

import Typing 

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.NonEmpty as NonEmptyParser

-- Uses megaparsec's lexeme parser to define something to break apart tokens on spaces and ignore comments
-- L.space expects:
--  A parser for space characters that does not accept empty input
--  A parser for line comments
--  A parser for block comments
skipSpace :: Parser ()
skipSpace = L.space 
    -- One or more spaces
    space1

    -- Skips from -- to the end of the line
    (L.skipLineComment "--")

    -- Skips from /* to */
    (L.skipBlockCommentNested "/*" "*/")

-- Uses the megaparsec lexer's lexeme function
-- It takes:
--  a way to handle whitespace after a lexeme
--  the parser
-- By not providing the parser here this provides a generic lexer function for any parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

data ParseErr = NotImplemented
    deriving (Enum, Eq, Ord, Show)

instance ShowErrorComponent ParseErr where
    showErrorComponent = show

type Parser = Parsec ParseErr String

-- Can think of it as a function from String to Either Error Bool
-- I mean it is afaik, but like I'm not sure
abool :: Parser Bool
-- Replaces the inner value of the Right arm of the Either monad with the correct value if matched
abool =  False <$ string "False" <|> True <$ string "True"

aint :: Parser Int
-- right of $ labels the Left arm of the either monad and left of the $ 
-- fmaps read (Haskell's parseInt) over the Right arm of the either monad if some numbers are read
aint = label "Integer" $ read <$> some numberChar

astr :: Parser String
-- non-greedily parses values from one double-quote to another
astr = between (char '"') (char '"') (takeWhileP (Just "String") (/= '"'))

aid :: Parser Identifier
aid = label "Identifier" $ do
    first <- letterChar <|> char '_' -- The first letter is a letter or an underscore
    
    -- The rest can be any non-whitespace character which I am expressing as either an alphanumeric
    -- or symbol character. This is repeated 0 to infinite times by applying many
    rest <- many $ alphaNumChar <|> symbolChar

    -- Constructs an identifier from the string repr of the identifier
    -- and then takes it into the Either monad with pure
    -- That last step is needed since in the do we're binding not fmapping
    pure $ Identifier $ first : rest

sexp :: Parser Sexpr
sexp = label "S-Expression" $ lexeme $ between 
    (lexeme (char '('))
    (char ')')
    (LSexpr <$> NonEmptyParser.some parseAny)

atom :: Parser Sexpr
atom = lexeme $ choice
    [
        ABool <$> abool, -- fmaps the SBool constructor over the Right of abool
        AInt <$> aint,
        AStr <$> astr,
        AIdent <$> aid
    ]

parseAny :: Parser Sexpr
parseAny = lexeme $ sexp <|> atom

parseSexpr :: String -> Either String Sexpr
parseSexpr input = 
    let output = parse (between skipSpace eof $ parseAny) "" input
    in
    case output of
        Left err -> Left $ errorBundlePretty err
        Right parsed -> Right parsed
