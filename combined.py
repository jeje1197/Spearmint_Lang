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
VAR         = 'var'

CLASS       = "class"

KEYWORDS = [
    IF, THEN,
    ELIF, ELSE, FOR, TO, 
    STEP, WHILE, FUNCTION, END,
    RETURN, CONTINUE, BREAK,

    VAR, CLASS
]

class Error:
    def __init__(self, message, position=None):
        self.message = message
        self.position = position

    # Returns a formatted string with an arrow pointing to
    # the place where an error occurs in the code
    def get_text_with_arrow(self):
        text = self.position.ftxt
        ln = self.position.ln
        col = self.position.col

        # Get the line with the error on it
        line_text = text.split('\n')[ln]

        # Point an arrow to where the error occurs
        arrow_str = ''
        while len(arrow_str) < col:
            arrow_str += ' '
        arrow_str += '^'

        return f'\t{line_text}\n\t{arrow_str}\n'


    def __str__(self) -> str:
        error_str = f'\n{self.message}\n\n'
        error_str += self.get_text_with_arrow()
        error_str += f'{self.position}\n'

        return error_str

    def __repr__(self) -> str:
        return self.__str__()

def is_error(object):
    return isinstance(object, Error)

class IllegalCharacterError(Error):
    def __init__(self, char, position=None):
        super().__init__(f"Illegal Character: '{char}'", position)
    
class InvalidSyntaxError(Error):
    def __init__(self, message, position):
        super().__init__(f'Invalid Syntax Error: {message}', position)

class RTError(Error):
    def __init__(self, message = None, position=None, context=None):
        super().__init__(f'Runtime Error: {message}', position)
        self.context = context

    def __repr__(self) -> str:
        return self.__str__()
    
    def __str__(self) -> str:
        string = self.generate_traceback()
        string += self.get_text_with_arrow()
        string += self.message
        return string

    def generate_traceback(self):
        result = ''
        pos = self.position
        ctx = self.context

        while ctx:
            result += f'\tFile <{pos.fn}>, line {str(pos.ln+1)}, col {str(pos.col+1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return f'Traceback (most recent callback):\n{result}' 



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

class Position:
    def __init__(self, idx, ln, col, file_name=None, file_text=None):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = file_name
        self.ftxt = file_text
    
    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0
        
        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

    def __repr__(self) -> str:
        return f'File: <{self.fn}>, line: {self.ln+1} col: {self.col+1}'

class Lexer:
    def __init__(self, fn, text):
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
            't': '\t'
        }

        while self.current_char != quote_char:
            if self.current_char == None:
                return Error(f"InvalidSyntaxError: Missing {quote_char}", start_pos)

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

class UnaryOpNode:
    def __init__(self, op_token, node) -> None:
        self.op_token = op_token
        self.node = node

        self.start_pos = self.op_token.start_pos
        self.end_pos = self.node.end_pos

    def __repr__(self):
        return f'(U: {self.op_token} {self.node})'

class BinaryOpNode:
    def __init__(self, left_node, op_token, right_node) -> None:
        self.left_node = left_node
        self.op_token = op_token
        self.right_node = right_node

        self.start_pos = self.left_node.start_pos
        self.end_pos = self.right_node.end_pos

    def __repr__(self):
        return f'({self.left_node} {self.op_token} {self.right_node})'

class NumberNode:
    def __init__(self, token) -> None:
        self.token = token

        self.start_pos = self.token.start_pos
        self.end_pos = self.token.end_pos

    def __repr__(self) -> str:
        return f'{self.token}'

class StringNode:
    def __init__(self, token) -> None:
        self.token = token

        self.start_pos = self.token.start_pos
        self.end_pos = self.token.end_pos

    def __repr__(self):
        return f'{self.token}'

class VarDeclarationNode:
    def __init__(self, var_name_token, expr_node):
        self.var_name_token = var_name_token
        self.expr_node = expr_node

    def __repr__(self):
        return f"(VarDeclaration: {self.var_name_token.value}: {self.expr_node}"

class VarAssignNode:
    def __init__(self, var_name_token, value_node) -> None:
        self.var_name_token = var_name_token
        self.value_node = value_node

        self.start_pos = self.var_name_token.start_pos
        self.end_pos = self.value_node.end_pos

    def __repr__(self):
        return f'(VarAssign: {self.var_name_token} = {self.value_node})'

class VarAccessNode:
    def __init__(self, var_name_token) -> None:
        self.var_name_token = var_name_token

        self.start_pos = self.var_name_token.start_pos
        self.end_pos = self.var_name_token.end_pos

    def __repr__(self):
        return f"(VarAccess: '{self.var_name_token.value}')"

class IfNode:
    def __init__(self, cases, else_statement_list) -> None:
        self.cases = cases
        self.else_statement_list = else_statement_list

        self.start_pos = self.cases[0][0].start_pos
        if self.else_statement_list:
            self.end_pos = self.else_statement_list[len(self.else_statement_list)-1].end_pos
        else:
            self.end_pos = self.cases[len(self.cases)-1][0].end_pos

    def __repr__(self) -> str:
        string = f'(If: {self.cases[0][0]} then: {self.cases[0][1]}'
        for i in range(1, len(self.cases)):
            string += f', else if: {self.cases[i][0]} then: {self.cases[i][1]}'
        if self.else_statement_list:
            string += f', else: {self.else_statement_list}'
        string += ')'
        return string
    
class ForNode:
    def __init__(self, init_statement, condition_node, update_statement, statement_list, start_pos, end_pos):
        self.init_statement = init_statement
        self.condition_node = condition_node
        self.update_statement = update_statement
        self.statement_list = statement_list
        
        self.start_pos = start_pos
        self.end_pos = end_pos

    def __repr__(self):
        return f'for ({self.init_statement}; {self.condition_node}; {self.update_statement}) do {self.statement_list}'

class WhileNode:
    def __init__(self, condition_node, statement_list):
        self.condition_node = condition_node
        self.statement_list = statement_list

        self.start_pos = self.condition_node.start_pos
        self.end_pos = self.statement_list[len(statement_list)-1].end_pos

    def __repr__(self):
        return f'while {self.condition_node} then {self.statement_list}'

class FunctionDefNode:
    def __init__(self, name_token, arg_names, statement_list, start_pos=None):
        self.name_token = name_token
        self.arg_names = arg_names
        self.statement_list = statement_list

        self.start_pos = start_pos
        self.end_pos = self.statement_list[len(statement_list)-1].end_pos

    def __repr__(self):
        return f'FunctionDef: {self.name_token.value}({self.arg_names}) do {self.statement_list}'

class FunctionCallNode:
    def __init__(self, name_token, args, end_pos=None):
        self.name_token = name_token
        self.args = args

        self.start_pos = self.name_token.start_pos
        self.end_pos = end_pos

    def __repr__(self):
        return f'FunctionCall: {self.name_token.value}({self.args})'

class ReturnNode:
    def __init__(self, expr_node, start_pos, end_pos):
        self.expr_node = expr_node

        self.start_pos = start_pos
        self.end_pos = self.expr_node.end_pos if expr_node else end_pos

    def __repr__(self):
        return f'Return: ({self.expr_node})'

class BreakNode:
    def __init__(self, start_pos, end_pos):
        self.start_pos = start_pos
        self.end_pos =  end_pos

    def __repr__(self):
        return f'Break'

class ContinueNode:
    def __init__(self, start_pos, end_pos):
        self.start_pos = start_pos
        self.end_pos =  end_pos

    def __repr__(self):
        return f'Continue'

class ClassDefNode:
    def __init__(self, class_name_token, property_list, start_pos, end_pos):
        self.class_name_token = class_name_token
        self.property_list = property_list

        self.start_pos = start_pos
        self.end_pos = end_pos
    
    def __repr__(self):
        return f"(ClassDef: {self.class_name_token.value} Properties: {self.property_list})"

class ClassAccessNode:
    def __init__(self, class_name_token, property_token, start_pos, end_pos):
        self.class_name_token = class_name_token
        self.property_token = property_token

        self.start_pos = start_pos
        self.end_pos = end_pos
    
    def __repr__(self):
        return f"(ClassAccess: {self.class_name_token.value}.{self.property_token.value})"

class TypeCastNode:
    def __init__(self, type_name_token) -> None:
        self.type_name_token = type_name_token

        self.start_pos = self.type_name_token.start_pos
        self.end_pos = self.type_name_token.end_pos
    
    def __repr__(self):
        return f"(TypeCast: {self.type_name_token.value})"


class Parser:
    def __init__(self, token_list) -> None:
        self.token_list = token_list
        self.index = -1
        self.current_token = None
        self.error = None
        self.get_next()

    def has_next(self, steps_ahead=1) -> bool:
        return self.index + steps_ahead < len(self.token_list)

    def get_next(self) -> Token or None:
        if self.has_next():
            self.index += 1
            self.current_token = self.token_list[self.index]
        else:
            self.current_token = None
        return self.current_token
    
    def look_ahead(self, steps_ahead) -> Token or None:
        if self.has_next(steps_ahead):
            return self.token_list[self.index + steps_ahead]
        else: return None

    # Returns list_of_statements, error
    def parse(self):
        statement_list = self.statements()
        if not self.error and self.current_token.type != Token.EOF:
            return None, Error("Unexpected Error: Did not reach end of file.", self.current_token.start_pos)

        return statement_list, self.error

    # Skips newline tokens
    def skip_newlines(self):
        while self.current_token.type == Token.NEWLINE:
            self.get_next()

    # Returns a list of statements
    def statements(self, end_search_at_token_type=Token.EOF) -> list:
        statement_list = []
        start_pos = self.current_token.start_pos

        # Skip newlines before
        self.skip_newlines()

        # Look for statements until end of file
        while self.current_token.type != end_search_at_token_type:
            # If end_token is not EOF and we reach end of file return
            # and let parser handle expected character after
            if self.current_token.type == Token.EOF: 
                return

            statement = self.statement()
            if self.error: return

            # Require newline after main statements if not end of file
            if not self.current_token.type in (Token.NEWLINE, end_search_at_token_type):
                self.error = InvalidSyntaxError("Expected newline or ';' after statement", statement.start_pos)
                return

            statement_list.append(statement)

            # Skip newlines
            self.skip_newlines()
        
        # if len(statement_list) == 0:
        #     self.error = InvalidSyntaxError('No valid statement found', start_pos)
        #     return

        return statement_list

    # Returns a node
    def statement(self):
        statement = None

        # VarDeclaration
        if self.current_token.matches(Token.KEYWORD, VAR):
            statement = self.var_declaration()
            if self.error: return
        
        # VarAssign or FunctionCall
        elif self.current_token.type == Token.IDENTIFIER:
            if self.current_token and self.look_ahead(1).type == Token.LPAREN:
                statement = self.function_call()
                if self.error: return
            elif self.current_token and self.look_ahead(1).type == Token.DOT:
                statement = self.class_access()
                if self.error: return
            else:
                statement = self.var_assign()
                if self.error: return

        # if statement
        elif self.current_token.matches(Token.KEYWORD, IF):
            statement = self.if_expr()
            if self.error: return

        # for statement
        elif self.current_token.matches(Token.KEYWORD, FOR):
            statement = self.for_expr()
            if self.error: return

        # while statement
        elif self.current_token.matches(Token.KEYWORD, WHILE):
            statement = self.while_expr()
            if self.error: return

        # function def statement
        elif self.current_token.matches(Token.KEYWORD, FUNCTION):
            statement = self.function_def()
            if self.error: return

        # return statement
        elif self.current_token.matches(Token.KEYWORD, RETURN):
            statement = self.return_statement()
            if self.error: return

        # break statement
        elif self.current_token.matches(Token.KEYWORD, BREAK):
            statement = self.break_statement()
            if self.error: return

        # continue statement
        elif self.current_token.matches(Token.KEYWORD, CONTINUE):
            statement = self.continue_statement()
            if self.error: return

        # class Def
        # elif self.current_token.matches(Token.KEYWORD, CLASS):
        #     statement = self.class_def()
        #     if self.error: return

        return statement

    def expr(self):
        ops = (Token.AND, Token.OR, Token.BITWISEAND, Token.BITWISEOR)
        node = self.bin_op(self.comp_expr, ops, self.comp_expr)
        if self.error: return 

        return node

    def comp_expr(self):
        # Check for '!'
        if self.current_token.type == Token.NOT:
            op_token = self.current_token
            self.get_next()

            self.skip_newlines()

            node = self.comp_expr()
            if self.error: return
            return UnaryOpNode(op_token, node)

        # Check for comparisons
        ops = (Token.EE, Token.NE, Token.LT, Token.GT, Token.LTE, Token.GTE)
        node = self.bin_op(self.arith_expr, ops, self.arith_expr)
        if self.error: return

        return node

    def arith_expr(self):
        ops = (Token.PLUS, Token.MINUS, Token.MOD)
        node = self.bin_op(self.term, ops, self.term)
        if self.error: return
        return node

    def term(self):
        ops = (Token.MUL, Token.DIV)
        node = self.bin_op(self.power, ops, self.power)
        if self.error: return
        return node

    def power(self):
        ops = (Token.POW)
        node = self.bin_op(self.atom, ops, self.power)
        if self.error: return
        return node

    def atom(self):
        token = self.current_token
        node_to_return = None

        if token.type in (Token.PLUS, Token.MINUS):
            self.get_next()
            atom = self.atom()
            node_to_return = UnaryOpNode(token, atom)

        # VarAccess or FunctionCall
        elif self.current_token.type == Token.IDENTIFIER:
            if self.look_ahead(1).type == Token.LPAREN:
                statement = self.function_call()
                if self.error: return
                node_to_return = statement
            elif self.look_ahead(1).type == Token.EQ:
                self.error = InvalidSyntaxError(f'Cannot assign variable mid expression',
                    token.start_pos)
                return
            else:
                self.get_next()
                node_to_return = VarAccessNode(token)
        
        elif token.type in (Token.INT, Token.FLOAT):
            self.get_next()
            node_to_return = NumberNode(token)

        elif token.type == Token.STRING:
            self.get_next()
            node_to_return = StringNode(token)

        elif token.type == Token.LPAREN:
            self.get_next()

            expr = self.expr()
            if self.error: return

            if self.current_token.type != Token.RPAREN:
                self.error = InvalidSyntaxError(f"Expected ')'", token.start_pos)
                return
            self.get_next()

            node_to_return = expr
        
        if not node_to_return:
            self.error = InvalidSyntaxError('Expected atom', token.start_pos)

        return node_to_return

    def bin_op(self, func_a, ops, func_b=None):
        if func_b == None: 
            func_b = func_a

        left_node = func_a()
        if self.error: return

        while self.current_token.type in ops:
            op_token = self.current_token
            self.get_next()

            self.skip_newlines()

            right_node = func_b()
            if self.error: return

            left_node = BinaryOpNode(left_node, op_token, right_node)

        return left_node

    def var_declaration(self):
        if not self.current_token.matches(Token.KEYWORD, VAR):
            self.error = InvalidSyntaxError("Expected keyword 'var'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError("Expected identifier", self.current_token.start_pos)
            return
        var_name_token = self.current_token
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.COLON:
            self.error = InvalidSyntaxError("Expected newline, semicolon or ':'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()
        
        expr_node = self.expr()
        if self.error: return
        return VarDeclarationNode(var_name_token, expr_node)

    def var_assign(self):
        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError('Expected Identifier', self.current_token.start_pos)
            return
        var_name_token = self.current_token
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.COLON:
            self.error = InvalidSyntaxError("Expected variable assignment ':'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        expr = self.expr()
        if self.error: return
        return VarAssignNode(var_name_token, expr)           

    def if_expr(self):
        cases = []

        #if (condition) {expr}
        if not self.current_token.matches(Token.KEYWORD, IF):
            self.error = InvalidSyntaxError(f"Expected '{IF}'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LPAREN:
            self.error = InvalidSyntaxError("Expected '('", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        condition = self.expr()
        if self.error: return
        self.skip_newlines()

        if self.current_token.type != Token.RPAREN:
            self.error = InvalidSyntaxError("Expected ')'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LBRACE:
            self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
            return
        self.get_next()

        statement_list = self.statements(Token.RBRACE)
        if self.error: return
        cases.append((condition, statement_list))

        if self.current_token.type != Token.RBRACE:
            self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        # elif (condition) {expr}
        while self.current_token.matches(Token.KEYWORD, ELSE) and self.look_ahead(1).matches(Token.KEYWORD, IF):
            self.get_next()
            self.get_next()
            self.skip_newlines()

            if self.current_token.type != Token.LPAREN:
                self.error = InvalidSyntaxError("Expected '('", self.current_token.start_pos)
                return
            self.get_next()
            self.skip_newlines()

            condition = self.expr()
            if self.error: return
            self.skip_newlines()

            if self.current_token.type != Token.RPAREN:
                self.error = InvalidSyntaxError("Expected ')'", self.current_token.start_pos)
                return
            self.get_next()
            self.skip_newlines()

            if self.current_token.type != Token.LBRACE:
                self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
                return
            self.get_next()

            statement_list = self.statements(Token.RBRACE)
            if self.error: return
            cases.append((condition, statement_list))

            if self.current_token.type != Token.RBRACE:
                self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
                return
            self.get_next()
            self.skip_newlines()

        else_statement_list = None
        # else {expr}
        if self.current_token.matches(Token.KEYWORD, ELSE):
            self.get_next()
            self.skip_newlines()

            if self.current_token.type != Token.LBRACE:
                self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
                return
            self.get_next()

            else_statement_list = self.statements(Token.RBRACE)
            if self.error: return

            if self.current_token.type != Token.RBRACE:
                self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
                return
            self.get_next()

        return IfNode(cases, else_statement_list)

    def for_expr(self):
        if not self.current_token.matches(Token.KEYWORD, FOR):
            self.error = InvalidSyntaxError(f"Expected '{FOR}'", self.current_token.start_pos)
            return
        start_pos = self.current_token.start_pos
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LPAREN:
            self.error = InvalidSyntaxError("Expected '('", self.current_token.start_pos)
            return
        self.get_next()

        init_statement = self.statement()
        
        if self.current_token.type != Token.NEWLINE:
            self.error = InvalidSyntaxError("Expected newline or semicolon", self.current_token.start_pos)
            return
        self.get_next()

        conditional_expr = self.expr()
        
        if not conditional_expr:
            self.error = InvalidSyntaxError("Expected conditional expr", self.current_token.start_pos)
            return
        
        if self.current_token.type != Token.NEWLINE:
            self.error = InvalidSyntaxError("Expected newline or semicolon", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        update_statement = self.statement()
        self.skip_newlines()

        if self.current_token.type != Token.RPAREN:
            self.error = InvalidSyntaxError("Expected ')'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LBRACE:
                self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
                return
        self.get_next()

        statement_list = self.statements(Token.RBRACE)
        if self.error: return

        self.skip_newlines()

        if self.current_token.type != Token.RBRACE:
            self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
            return
        end_pos = self.current_token.start_pos
        self.get_next()

        return ForNode(init_statement, conditional_expr, update_statement, 
            statement_list, start_pos, end_pos)

    def while_expr(self):
        if not self.current_token.matches(Token.KEYWORD, WHILE):
            self.error = InvalidSyntaxError(f"Expected '{WHILE}'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LPAREN:
            self.error = InvalidSyntaxError("Expected '('", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        cond_node = self.expr()
        if self.error: return
        self.skip_newlines()

        if self.current_token.type != Token.RPAREN:
            self.error = InvalidSyntaxError("Expected ')'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LBRACE:
                self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
                return
        self.get_next()

        statement_list = self.statements(Token.RBRACE)
        if self.error: return

        if self.current_token.type != Token.RBRACE:
            self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
            return
        self.get_next()

        return WhileNode(cond_node, statement_list)

    def function_def(self):
        arg_names = []
        statement_list = []
        start_pos = self.current_token.start_pos

        if not self.current_token.matches(Token.KEYWORD, FUNCTION):
            self.error = InvalidSyntaxError('Expected function', self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError("Expected identifier", self.current_token.start_pos)
            return
        name_token = self.current_token
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LPAREN:
            self.error = InvalidSyntaxError("Expected '('", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        # Check if there are any arguments
        if self.current_token.type == Token.IDENTIFIER:
            arg_names.append(self.current_token.value)
            self.get_next()
            self.skip_newlines()

            # Check for more arguments
            while self.current_token.type == Token.COMMA:
                comma_start_pos = self.current_token.start_pos
                self.get_next()
                self.skip_newlines()

                if self.current_token.type != Token.IDENTIFIER:
                    self.error = InvalidSyntaxError('Expected identifier after comma', comma_start_pos)
                    return

                arg_names.append(self.current_token.value)
                self.get_next()
                self.skip_newlines()

            # Check for following arguments without separating commas
            if self.current_token.type == Token.IDENTIFIER:
                self.error = InvalidSyntaxError('Expected comma before identifier', self.current_token.start_pos)
                return

        self.skip_newlines()

        if self.current_token.type != Token.RPAREN:
            self.error = InvalidSyntaxError("Expected ')'", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LBRACE:
                self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
                return
        self.get_next()

        statement_list = self.statements(Token.RBRACE)
        if self.error: return

        if self.current_token.type != Token.RBRACE:
                self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
                return
        self.get_next()

        return FunctionDefNode(name_token, arg_names, statement_list ,start_pos)
        
    def function_call(self):
        args = []
        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError("Expected identifier", self.current_token.start_pos)
            return
        name_token = self.current_token
        self.get_next()

        if self.current_token.type != Token.LPAREN:
            self.error = InvalidSyntaxError("Expected '('", self.current_token.start_pos)
            return
        self.get_next()
        self.skip_newlines()

        # Check for any arguments
        expr = self.expr()
        if self.error and self.current_token.type != Token.RPAREN:
            return
        else:
            self.error = None
        self.skip_newlines()

        # If there is an arguments, check for more
        if expr:
            args.append(expr)

            while self.current_token.type == Token.COMMA:
                self.get_next()

                self.skip_newlines()

                expr = self.expr()
                if self.error: return

                args.append(expr)
                self.skip_newlines()

        if self.current_token.type != Token.RPAREN:
            self.error = InvalidSyntaxError("Expected ')'", self.current_token.start_pos)
            return
        end_pos = self.current_token.end_pos
        self.get_next()

        return FunctionCallNode(name_token, args, end_pos)

    def return_statement(self):
        if not self.current_token.matches(Token.KEYWORD, RETURN):
            self.error = InvalidSyntaxError('Expected return', self.current_token.start_pos)
            return
        start_pos = self.current_token.start_pos
        end_pos = self.current_token.end_pos
        self.get_next()

        expr = None
        if not self.current_token.type in (Token.NEWLINE):
            expr = self.expr()
            if self.error: return

        return ReturnNode(expr, start_pos, end_pos)

    def break_statement(self):
        if not self.current_token.matches(Token.KEYWORD, BREAK):
            self.error = InvalidSyntaxError('Expected break', self.current_token.start_pos)
            return
        start_pos = self.current_token.start_pos
        end_pos = self.current_token.end_pos
        self.get_next()

        return BreakNode(start_pos, end_pos)

    def continue_statement(self):
        if not self.current_token.matches(Token.KEYWORD, CONTINUE):
            self.error = InvalidSyntaxError('Expected continue', self.current_token.start_pos)
            return
        start_pos = self.current_token.start_pos
        end_pos = self.current_token.end_pos
        self.get_next()

        return ContinueNode(start_pos, end_pos)

    def class_def(self):
        if not self.current_token.matches(Token.KEYWORD, CLASS):
            self.error = InvalidSyntaxError('Expected class', self.current_token.start_pos)
            return
        start_pos = self.current_token.start_pos
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError("Expected class name", self.current_token.start_pos)
            return
        name_token = self.current_token
        self.get_next()
        self.skip_newlines()

        if self.current_token.type != Token.LBRACE:
                self.error = InvalidSyntaxError("Expected '{'", self.current_token.start_pos)
                return
        self.get_next()

        property_list = self.statements(Token.RBRACE)
        if self.error: return

        if self.current_token.type != Token.RBRACE:
            self.error = InvalidSyntaxError("Expected '}'", self.current_token.start_pos)
            return
        end_pos = self.current_token.start_pos
        self.get_next()

        return ClassDefNode(name_token, property_list, start_pos, end_pos)

    def class_access(self):
        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError("Expected object name", self.current_token.start_pos)
            return
        name_token = self.current_token
        start_pos = self.current_token.start_pos
        self.get_next()

        if self.current_token.type != Token.DOT:
            self.error = InvalidSyntaxError("Expected '.'", self.current_token.start_pos)
            return
        self.get_next()

        if self.current_token.type != Token.IDENTIFIER:
            self.error = InvalidSyntaxError("Expected object field name", self.current_token.start_pos)
            return
        field_name_token = self.current_token
        self.get_next()

        return ClassAccessNode(name_token, field_name_token, start_pos, self.current_token)


class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None
    
    def set_symbol_table(self, symbol_table):
        self.symbol_table = symbol_table


class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    # Looks for key in local symbol table. If not found, checks parents for key.
    # If neither are found, returns None
    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def get_locally(self, name):
        return self.symbols.get(name, None)

    # Returns the value and the lowest context containing the key
    def get_with_lowest_context(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get_with_lowest_context(name)
        return value, self

    # Sets the key-value pair in the lowest context containing the key.
    # If no contexts contain the key, sets the key-value pair in the current context.
    def set(self, name, value):
        # Check if defined in scope
        value_in_lowest_context, symbol_table_of_lowest_context = self.get_with_lowest_context(name)
        if value_in_lowest_context:
            symbol_table_of_lowest_context.symbols[name] = value
            return True # If setting value of higher context
        else:
            self.symbols[name] = value
            return False # If setting in local context

    def set_locally(self, name, value):
        self.symbols[name] = value

    def set_multiple(self, list):
        for name, value in list:
            self.symbols[name] = value

    # Removes the key-value pair from the current context
    def remove(self, name):
        del self.symbols[name]

    def __repr__(self) -> str:
        return str(self.symbols)


class Value:
    def __init__(self, value, type):
        self.value = value
        self.type = type
        self.context = None

    def set_pos(self, start_pos=None, end_pos=None):
        self.start_pos = start_pos
        self.end_pos = end_pos
        return self

    def set_context(self, context):
        self.context = context
        return self

    def added_to(self, other):
        return None, self.illegal_operation(other)

    def subbed_by(self, other):
        return None, self.illegal_operation(other)

    def multed_by(self, other):
        return None, self.illegal_operation(other)

    def dived_by(self, other):
        return None, self.illegal_operation(other)

    def powed_by(self, other):
        return None, self.illegal_operation(other)

    def modded_by(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_eq(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparison_ne(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lt(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparison_gt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lte(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gte(self, other):
        return None, self.illegal_operation(other)

    def anded_by(self, other):
        return None, self.illegal_operation(other)

    def ored_by(self, other):
        return None, self.illegal_operation(other)

    def notted(self):
        return None, self.illegal_operation()

    def copy(self):
        raise Exception('No copy method defined')

    def is_true(self):
        return bool(self.value)

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return self.__str__()

    def illegal_operation(self, other=None):
        if not other: other = self
        return RTError(f'Illegal operation on {self.type}', other.start_pos, self.context)

    def get_type(self):
        return self.type

class Number(Value):
    def __init__(self, value):
        super().__init__(value, type="Number")

    def added_to(self, other):
        if isinstance(other, Number): 
            result = self.value + other.value
            return Number(result), None
        elif isinstance(other, String): 
            result = str(self.value) + other.value
            return String(result), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError("Division by zero", other.start_pos, self.context)
            return Number(self.value / other.value), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def modded_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError("Modded by zero", other.start_pos, self.context)
            return Number(self.value % other.value), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)
    
    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)
    
    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(1 if (self.value and other.value) else 0).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        return copy

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)

class String(Value):
    def __init__(self, value):
        super().__init__(value, "String")

    def added_to(self, other):
        return String(self.value + str(other.value)), None

    def get_comparison_eq(self, other):
        if isinstance(other, String):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)
    
    def get_comparison_ne(self, other):
        if isinstance(other, String):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.start_pos, self.end_pos)
        copy.set_context(self.context)
        return copy


class Function:
    def __init__(self, name, arg_names, statement_list, built_in=False) -> None:
        self.name = name
        self.arg_names = arg_names
        self.statement_list = statement_list
        self.built_in = built_in

    def get_name(self):
        return self.name

    def check_args(self, args_passed):
        num_args_passed = len(args_passed)
        num_args_in_def = len(self.arg_names)

        if num_args_passed < num_args_in_def:
            raise Exception(
                f'function {self.name} is missing {num_args_in_def - num_args_passed} argument{"s" if num_args_in_def - num_args_passed > 1 else ""}'
                )
        elif num_args_passed > num_args_in_def:
            raise Exception(f'function {self.name} has {num_args_passed - num_args_in_def} too many arguments')
        return True

    def execute(self):
        raise Exception(f'{self.name}.execute method is undefined.')

    def __repr__(self):
        return f'(Function Name: "{self.name}", Statements: {self.statement_list})'

class Class(Value):
    def __init__(self, class_name):
        self.class_name = class_name
        self.fields = SymbolTable()
        self.functions = SymbolTable()

    def add_field(self, field_name, value):
        self.fields.set(field_name, value)

    def add_method(self, method_name, value):
        if self.functions.get(method_name):
            return self.no_function_redefinition(method_name)
        self.functions.set(method_name, value)

    def access_field(self, field_name):
        field = self.fields.get(field_name)
        if not field:
            return self.no_property_found(field_name)
        return field

    def access_method(self, method_name):
        method = self.functions.get(method_name)
        if not method:
            return self.no_property_found(method_name)
        return method

    def no_function_redefinition(self, property_name):
        raise Exception(f'{property_name} property is already defined in {self.class_name}.')

    def no_property_found(self, property_name):
        raise Exception(f'No {property_name} property defined in {self.class_name}.')

    def copy(self):
        copy = Class(self.class_name)
        copy.fields = self.fields
        copy.functions = self.functions
        return copy

    def __repr__(self) -> str:
        return f'(Class Name: "{self.class_name}"\n\tFields: {self.fields}\n\tMethods: {self.functions}\n)' 


# Print method
print_args = ['text']

# Print function implementation
def execute_print(interpreter, context):
    value = str(context.symbol_table.get('text'))
    print(value)
    interpreter.output += value

print_function = Function('print', print_args, statement_list=None, built_in=True)
print_function.execute = execute_print

built_in_functions = [print_function]

class Interpreter:
    def __init__(self, context):
        self.error = None
        self.global_context = context
        self.function_symbol_table = SymbolTable()

        self.should_return = False
        self.return_value = None
        self.should_break = False
        self.should_continue = False

        self.output = ""

    def add_BuiltInFunctions(self):
        for function in built_in_functions:
            self.function_symbol_table.set(function.name, function)

    # Creates child context from parent context
    def generate_new_context(self, context_name, parent_context, parent_entry_pos=None):
        new_context = Context(context_name, parent_context, parent_entry_pos)
        new_context.set_symbol_table(SymbolTable(parent_context.symbol_table))
        return new_context
    
    # Visits a node function according to its type name and returns Object or None
    def visit(self, node, context) -> object or None:
        method_name = f"visit_{type(node).__name__}"
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    # Raises exception if no method defined for node
    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_list(self, statement_list, context):
        for node in statement_list:
            self.visit(node, context)
            if self.should_return: return
            if self.should_break: return
            if self.should_continue: return
            if self.error: return

    # Performs an operation between two nodes
    def visit_BinaryOpNode(self, node, context):
        left = self.visit(node.left_node, context)
        if self.error: return
        right = self.visit(node.right_node, context)
        if self.error: return
        
        error, result = None, None
        if node.op_token.type == Token.PLUS:
            result, error = left.added_to(right)
        elif node.op_token.type == Token.MINUS:
            result, error = left.subbed_by(right)
        elif node.op_token.type == Token.MUL:
            result, error = left.multed_by(right)
        elif node.op_token.type == Token.DIV:
            result, error = left.dived_by(right)
        elif node.op_token.type == Token.POW:
            result, error = left.powed_by(right)
        elif node.op_token.type == Token.MOD:
            result, error = left.modded_by(right)
        elif node.op_token.type == Token.EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_token.type == Token.NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_token.type == Token.LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_token.type == Token.GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_token.type == Token.LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_token.type == Token.GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_token.type == Token.AND:
            result, error = left.anded_by(right)
        elif node.op_token.type == Token.OR:
            result, error = left.ored_by(right)
        elif node.op_token.type == Token.NOT:
            result, error = left.notted(right)

        if error:
            error.position = node.op_token.start_pos
            self.error = error
            return
        else:
            return result

    # Creates a Number object from a NumberNode
    def visit_NumberNode(self, node, context):
        return Number(node.token.value).set_context(context).set_pos(node.start_pos, node.end_pos)

    # Creates a String object from a StringNode
    def visit_StringNode(self, node, context):
        return String(node.token.value).set_context(context).set_pos(node.start_pos, node.end_pos)

    # Declares a variable
    def visit_VarDeclarationNode(self, node, context):
        var_name = node.var_name_token.value
        if context.symbol_table.get_locally(var_name):
            self.error = RTError(f'{var_name} has already been defined in scope.',
                node.var_name_token.start_pos)
            return

        value = self.visit(node.expr_node, context)
        if self.error: return

        context.symbol_table.set_locally(var_name, value)

    # Assigns a variable a value
    def visit_VarAssignNode(self, node, context):
        var_name = node.var_name_token.value
        if not context.symbol_table.get(var_name):
            self.error = RTError(f"{var_name} hasn't been declared yet.", node.var_name_token.start_pos)
            return

        value = self.visit(node.value_node, context)
        if self.error: return

        context.symbol_table.set(var_name, value)
        return value

    # Accesses variable if defined in scope of JPL code
    def visit_VarAccessNode(self, node, context):
        var_name = node.var_name_token.value
        value = context.symbol_table.get(var_name)

        if value:
            return value.copy().set_context(context)
        else:
            self.error = RTError(f"{var_name} is undefined", node.start_pos, context)
            return

    # Runs an if statement from an IfNode
    def visit_IfNode(self, node, context):
        for condition, statement_list in node.cases:
            condition_value = self.visit(condition, context)
            if self.error: return

            if condition_value.is_true():
                self.visit(statement_list, context)
                if self.error: return
                return

        if node.else_statement_list:
            self.visit(node.else_statement_list, context)
            if self.error: return

    # Runs a for loop from a ForNode
    def visit_ForNode(self, node, context):
        condition_context = self.generate_new_context(f'For Loop (Condition)', context)
        self.visit(node.init_statement, condition_context)

        while self.visit(node.condition_node, condition_context).is_true():
            body_context = self.generate_new_context(f'For Loop (Body)', condition_context)
            
            self.visit(node.statement_list, body_context)
            if self.error: return
            if self.should_return:
                return
            if self.should_break:
                self.should_break = False
                return
            if self.should_continue:
                self.should_continue = False

            self.visit(node.update_statement, condition_context)

    # Runs a while loop from a WhileNode
    def visit_WhileNode(self, node, context):

        while self.visit(node.condition_node, context).is_true():
            if self.error: return
            # Create new context for local variables
            body_context = self.generate_new_context(f'While Loop (body)', context)
            
            self.visit(node.statement_list, body_context)
            if self.error: return
            if self.should_return:
                return
            if self.should_break:
                self.should_break = False
                return
            if self.should_continue:
                self.should_continue = False

    # Add new Function to function symbol table
    def visit_FunctionDefNode(self, node, context):
        function_name = node.name_token.value
        if self.function_symbol_table.get(function_name):
            return RTError(f'function {function_name} is already defined', node.start_pos, context)

        self.function_symbol_table.set(function_name, Function(function_name, node.arg_names, node.statement_list))

    # Checks function symbol table for valid FunctionDef then executes
    def visit_FunctionCallNode(self, node, context):
        function_name = node.name_token.value
        function = self.function_symbol_table.get(function_name)
        if not function:
            return RTError(f'function {function_name} is not defined', node.name_token.start_pos)

        # Check for same number of args as defined in function
        function.check_args(node.args)

        # Add args to new context
        new_context = self.generate_new_context(f'Function: {function.name}()', context)

        # Map argument variables to values passed in
        for i in range(len(function.arg_names)):
            value = self.visit(node.args[i], context)
            if self.error: return
            if not value:
                self.error = RTError(f"Function {function.name} argument '{function.arg_names[i]}' did not receive a value", node.args[i].start_pos,
                context)
                return
            new_context.symbol_table.set_locally(function.arg_names[i], value)
            

        # If the function is in the standard library, call its execute method
        if function.built_in:
            function.execute(self, new_context)
            return

        self.visit(function.statement_list, new_context)
        if self.error: return
        if self.should_return:
            return_val = self.return_value 
            self.should_return = False
            self.return_value = None
            return return_val

    def visit_ReturnNode(self, node, context):
        if node.expr_node:
            self.return_value = self.visit(node.expr_node, context)
        self.should_return = True

    def visit_BreakNode(self, node, context):
        self.should_break = True

    def visit_ContinueNode(self, node, context):
        self.should_continue = True


def run(file_name, input_text, compile=False):

    # Lexer
    lexer = Lexer(file_name, input_text)
    token_list, error = lexer.get_tokens()

    if error: 
        print(error)
        return error
    # else: 
    #     print(f'List of Tokens: {token_list}')


    # Parser
    parser = Parser(token_list)
    statement_list, error = parser.parse()

    if error: 
        print(error)
        return error
    else:
        print(f'----- Statements: {len(statement_list)} -----')
        for statement in statement_list:
            print(statement)
        print('----------')


    global_symbol_table = SymbolTable()
    global_symbol_table.set_multiple([
        ("true", Number(1)),
        ("false", Number(0))
    ])
    global_context = Context(file_name)
    global_context.set_symbol_table(global_symbol_table)

    #     #Interpreter
    interpreter = Interpreter(global_context)
    interpreter.add_BuiltInFunctions()
    interpreter.visit(statement_list, global_context)

    if interpreter.error:
        print(interpreter.error)
        return interpreter.error
    else:
        return interpreter.output


if __name__ == '__main__':
    run('Console', "print(2*2)", False)