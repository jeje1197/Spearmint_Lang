# Imports
from .Token import Token
from .Position import Position
from error.Error import Error, IllegalCharacterError, is_error
import string

# Constants
WHITESPACE  = " \t"
NEWLINE     = ";\n"
LETTERS     = string.ascii_letters
DIGITS      = string.digits
LETTERS_DIGITS = LETTERS + DIGITS
QUOTATIONS  = "'`" + '"'

# Keywords
IF          = "if"
THEN        = "then"
ELIF        = "elif"
ELSE        = "else"

FOR         = "for"
TO          = "to"
STEP        = "step"
WHILE       = "while"

FUNCTION    = "fun"
END         = "end"

RETURN      = "return"
CONTINUE    = "continue"
BREAK       = "break"

# Data Types
VAR         = "var"

CLASS       = "class"


KEYWORDS = [
    IF, THEN,
    ELIF, ELSE, FOR, TO, 
    STEP, WHILE, FUNCTION, END,
    RETURN, CONTINUE, BREAK,

    VAR, CLASS
]

class Lexer:
    def __init__(self, fn=None, text=""):
        self.fn = fn
        self.text = text
        self.index = -1
        self.position = Position(-1, 0, -1, fn, text)
        self.current_char = None

        self.get_next()

    # Returns true is there are more tokens to create
    def has_next(self, steps_ahead=1) -> bool:
        return self.index + steps_ahead < len(self.text)

    # Gets next character from file text
    # If no characters are left, returns None
    def get_next(self):
        if self.has_next():
            self.index += 1
            self.position.advance(self.current_char)
            self.current_char = self.text[self.index]
        else: self.current_char = None
        return self.current_char

    # Gets next character from file text
    # If no characters are left, returns None
    def look_ahead(self, steps_ahead):
        if self.has_next(steps_ahead):
            return self.text[self.index + steps_ahead]
        else: return None

    # Read through file and create tokens (Ignore whitespace)
    # returns [token_array, Error]
    def get_tokens(self):
        tokens = []

        while (self.current_char != None):
            char = self.current_char

            if char in WHITESPACE:
                self.get_next()
            elif char == "/":
                token = self.make_div_or_comment()
                if token: tokens.append(token)
            elif char in NEWLINE:
                tokens.append(Token(Token.NEWLINE, char, self.position))
                self.get_next()
            elif char in LETTERS:
                tokens.append(self.make_id_or_keyword_token())
            elif char in DIGITS:
                tokens.append(self.make_number_token())
            elif char in QUOTATIONS:
                tokens.append(self.make_string_token())
            elif char == '(':
                tokens.append(Token(Token.LPAREN, char, self.position))
                self.get_next()
            elif char == ')':
                tokens.append(Token(Token.RPAREN, char, self.position))
                self.get_next()
            elif char == '{':
                tokens.append(Token(Token.LBRACE, char, self.position))
                self.get_next()
            elif char == '}':
                tokens.append(Token(Token.RBRACE, char, self.position))
                self.get_next()
            elif char == '[':
                tokens.append(Token(Token.LBRACKET, char, self.position))
                self.get_next()
            elif char == ']':
                tokens.append(Token(Token.RBRACKET, char, self.position))
                self.get_next()
            elif char == ':':
                tokens.append(Token(Token.COLON, char, self.position))
                self.get_next()
            elif char == ';':
                tokens.append(Token(Token.SEMICOLON, char, self.position))
                self.get_next()
            elif char == ',':
                tokens.append(Token(Token.COMMA, char, self.position))
                self.get_next()
            elif char == '.':
                tokens.append(Token(Token.DOT, char, self.position))
                self.get_next()
            elif char == '+':
                tokens.append(Token(Token.PLUS, char, self.position))
                self.get_next()
            elif char == '-':
                tokens.append(self.make_minus_or_arrow_token())
            elif char == '*':
                tokens.append(Token(Token.MUL, char, self.position))
                self.get_next()
            elif char == '^':
                tokens.append(Token(Token.POW, char, self.position))
                self.get_next()
            elif char == '%':
                tokens.append(Token(Token.MOD, char, self.position))
                self.get_next()
            elif char == '&':
                tokens.append(self.make_and_token())
            elif char == '|':
                tokens.append(self.make_or_token())
            elif char == '!':
                tokens.append(self.make_not_token())
            elif char == '=':
                tokens.append(self.make_equals_token())
            elif char == '<':
                tokens.append(self.make_less_than_token())
            elif char == '>':
                tokens.append(self.make_greater_than_token())
            else:
                tokens.append(IllegalCharacterError(char, self.position))

            # Check if the most recent element in tokens array is an Error
            if len(tokens) > 0 and is_error(tokens[len(tokens)-1]):
                return [None, tokens[len(tokens)-1]]

        tokens.append(Token(Token.EOF, "EOF", self.position.advance()))
        return [tokens, None]


    def make_div_or_comment(self):
        start_pos = self.position.copy()
        self.get_next()

        # If second '/' is found, skip line
        if self.current_char == '/':
            while not self.current_char in ('\n', None):
                self.get_next()

            self.get_next()
            return None
        else: return Token(Token.DIV, '/', start_pos)


    def make_id_or_keyword_token(self):
        id_str = ''
        start_pos = self.position.copy()

        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.get_next()

        token_type = Token.KEYWORD if id_str in KEYWORDS else Token.IDENTIFIER
        return Token(token_type, id_str, start_pos, self.position)

 
    def make_number_token(self):
        num_str = self.current_char
        start_pos = self.position.copy()

        dot_count = 0
        self.get_next()
        while self.current_char != None and self.current_char in DIGITS + '.':
            # Decimals
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1
            
            num_str += self.current_char
            self.get_next()

        if num_str[len(num_str)-1] == '.':
            return Error('InvalidSyntaxError: Expected digits after decimal', self.position)

        if dot_count == 0:
            return Token(Token.INT, int(num_str), start_pos, self.position)
        else:
            return Token(Token.FLOAT, float(num_str), start_pos, self.position)


    def make_string_token(self):
        quote_char = self.current_char
        string = ''
        start_pos = self.position.copy()
        self.get_next()

        # Map for escape characters
        escape_char = {
            'n': '\n',
            't': '\t',
            '"': '"',
            '\\': '\\'
        }

        while self.current_char != quote_char:
            if self.current_char == None:
                return Error(f"InvalidSyntaxError: Missing '{quote_char}'", start_pos)

            # Check for escape characters:
            # a '\' followed by 'n' or 't' and append
            if self.current_char == '\\' and self.look_ahead(1) in escape_char:
                self.get_next()
                string += escape_char.get(self.current_char)
            else:
                string += self.current_char
            self.get_next()

        self.get_next()
        return Token(Token.STRING, string, start_pos, self.position)


    def make_minus_or_arrow_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == '>':
            self.get_next()
            return Token(Token.ARROW, '->',  start_pos, self.position)

        return Token(Token.MINUS, '-', start_pos, self.position)


    def make_and_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == '&':
            self.get_next()
            return Token(Token.AND, '&&', start_pos, self.position)

        return Token(Token.BITWISEAND, '&', start_pos, self.position)


    def make_or_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == '|':
            self.get_next()
            return Token(Token.OR, '||', start_pos, self.position)

        return Token(Token.BITWISEOR, '|', start_pos, self.position)


    def make_not_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == '=':
            self.get_next()
            return Token(Token.NE, "!=", start_pos, self.position)

        return Token(Token.NOT, "!", start_pos, self.position)


    def make_equals_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == "=":
            self.get_next()
            return Token(Token.EE, "==", start_pos, self.position)

        return Token(Token.EQ, "=", start_pos)


    def make_less_than_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == "=":
            self.get_next()
            return Token(Token.LTE, "<=", start_pos, self.position)

        return Token(Token.LT, "<", start_pos)


    def make_greater_than_token(self):
        start_pos = self.position.copy()
        self.get_next()

        if self.current_char == "=":
            self.get_next()
            return Token(Token.GTE, ">=", start_pos, self.position)

        return Token(Token.GT, ">", start_pos)