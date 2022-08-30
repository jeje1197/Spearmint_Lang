from typing import List
from error.Error import Error, InvalidSyntaxError
from lexer.Lexer import BREAK, CLASS, CONTINUE, ELSE, FOR, FUNCTION, IF, RETURN, STEP, TO, VAR, WHILE
from lexer.Token import Token
from parser.Nodes import BinaryOpNode, BreakNode, ClassAccessNode, ClassDefNode, ContinueNode, ForNode, FunctionCallNode, FunctionDefNode, IfNode, NumberNode, ReturnNode, StringNode, UnaryOpNode, VarAccessNode, VarAssignNode, VarDeclarationNode, WhileNode

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
    def statements(self, end_search_at_token_type=Token.EOF) -> List or None:
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
