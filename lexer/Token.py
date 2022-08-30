# Token Class
class Token:
    def __init__(self, type, value, start_pos=None, end_pos=None):
        self.type = type
        self.value = value

        if start_pos:
            self.start_pos = start_pos.copy()
            self.end_pos = start_pos.copy()
            self.end_pos.advance()
        
        if end_pos:
            self.end_pos = end_pos.copy()

    # Can match any number of values in category
    def matches(self, type, value):
        return self.type == type and (self.value == value or self.value in value)
    
    def __repr__(self):
        if not self.type in (Token.INT, Token.FLOAT):
            return f"({self.type}: '{self.value}')"
        else:
            return f'({self.type}: {self.value})'

    # Token Types
    FLOAT    = "float"
    INT      = "int"
    VOID        = "void"
    IDENTIFIER = "identifier"
    KEYWORD  = "KEYWORD"
    STRING   = "string"
    LPAREN   = "LPAREN"
    RPAREN   = "RPAREN"
    LBRACE = "LEFT BRACE"
    RBRACE = "RIGHT BRACE"
    LBRACKET = "LEFT BRACKET"
    RBRACKET = "RIGHT BRACKET"
    NEWLINE  = "NEWLINE"
    EOF      = "EOF"
    COLON    = "COLON"
    SEMICOLON = "SEMICOLON"
    DOT      = "DOT"
    COMMA    = "COMMA"
    ARROW    = "ARROW"

    PLUS     = "PLUS"
    MINUS    = "MINUS"
    MUL      = "MULTIPLY"
    DIV      = "DIVIDE"
    POW      = "POW"
    MOD      = "MOD"

    INCREMENT = "INCREMENT"
    DECREMENT = "DECREMENT"

    EQ       = "EQUALS"

    AND      = "AND"
    OR       = "OR"
    BITWISEAND = "BITWISEAND"
    BITWISEOR  = "BITWISEOR"
    NOT      = "NOT"
    NE       = "NOT EQUALS"
    EE       = "EQUALS EQUALS"
    LT       = "LESS THAN"
    GT       = "GREATER THAN"
    LTE      = "LESS THAN OR EQUALS"
    GTE      = "GREATER THAN OR EQUALS"