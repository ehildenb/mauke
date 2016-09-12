
module GLL.Combinators.Lexer (
    default_lexer, lexer, LexerSettings(..), emptyLanguage,
    ) where

import GLL.Types.Abstract (Token(..), SubsumesToken(..))
import Data.Char (isSpace, isDigit, isAlpha, isUpper, isLower)
import Text.Regex.Applicative

-- | Settings for changing the behaviour of the builtin lexer 'lexer'.
-- Lexers are built using "Text.Regex.Applicative".
data LexerSettings = LexerSettings {
        -- | Which keychars to recognise? Default: none.
        keychars        :: [Char]
        -- | Which keywords to recognise? Default: none.
    ,   keywords        :: [String]
        -- | What is considered a whitespace character? Default: 'Data.Char.isSpace'.
    ,   whitespace      :: Char -> Bool
        -- | How does a line comment start? Default: '"'//'"'.
    ,   lineComment     :: String
        -- | How to recognise identifiers? Default alphanumerical with lowercase alpha start.
    ,   identifiers     :: RE Char String
        -- | How to recognise alternative identifiers? Default alphanumerical with uppercase alpha start.
    ,   altIdentifiers  :: RE Char String
        -- | Arbitrary tokens /(a,b)/. /a/ is the token name, /b/ is a regular expression.
    ,   tokens          :: [(String, RE Char String)]
    }

-- | The default 'LexerSettings'.
emptyLanguage :: LexerSettings
emptyLanguage = LexerSettings [] [] isSpace "//"
    ((:) <$> psym isLower <*> lowercase_id)
    ((:) <$> psym isUpper <*> lowercase_id)
    []
 where lowercase_id = many (psym (\c -> isAlpha c || c == '_' || isDigit c))

-- | A lexer using the default 'LexerSettings'.
default_lexer :: SubsumesToken t => String -> [t]
default_lexer = lexer emptyLanguage 

-- | A lexer parameterised by 'LexerSettings'.
lexer :: SubsumesToken t => LexerSettings -> String -> [t]
lexer _ [] = []
lexer lexsets s =
    let re =    (Just <$> lTokens lexsets)
            <|> (Nothing <$ some (psym (whitespace lexsets)))
            <|> (Nothing <$ string (lineComment lexsets) <* many (psym ((/=) '\n')))
    in case findLongestPrefix re s of
        Just (Just tok, rest)   -> tok : lexer lexsets rest
        Just (Nothing,rest)     -> lexer lexsets rest
        Nothing                 -> error ("lexical error at: " ++ show (take 10 s))

lTokens :: SubsumesToken t => LexerSettings -> RE Char t 
lTokens lexsets =
        lCharacters
    <|> lKeywords
    <|> charsToInt  <$> optional (sym '-') <*> some (psym isDigit)
    <|> upcast . IDLit . Just <$> identifiers lexsets 
    <|> upcast . AltIDLit . Just <$> altIdentifiers lexsets
    <|> upcast . CharLit . Just <$> lCharLit
    <|> upcast . StringLit . Just <$> lStringLit
    <|> lMore
    where   
            charsToInt Nothing n = upcast (IntLit (Just (read n)))
            charsToInt (Just _) n = upcast (IntLit (Just (-(read n))))

            lChar c = upcast (Char c) <$ sym c
            lCharacters = foldr ((<|>) . lChar) empty (keychars lexsets) 

            lKeyword k  = upcast (Keyword k) <$ string k
            lKeywords = foldr ((<|>) . lKeyword) empty (keywords lexsets)

            lMore = foldr ((<|>) . uncurry lToken) empty (tokens lexsets)
            lToken t re = upcast . Token t . Just <$> re

            lStringLit = toString <$ sym '\"' <*> many strChar <* sym '\"'
             where strChar =  sym '\\' *> sym '\"'
                              <|> psym ((/=) '\"')
                   toString inner = read ("\"" ++ inner ++ "\"")

            lCharLit = id <$ sym '\'' <*> charChar <* sym '\''
              where charChar = sym '\\' *> sym '\''
                                <|> psym ((/=) '\'')
