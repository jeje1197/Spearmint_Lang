from tracemalloc import start


class UnaryOpNode:
    def __init__(self, op_token, expr_node) -> None:
        self.op_token = op_token
        self.expr_node = expr_node

        self.start_pos = self.op_token.start_pos
        self.end_pos = self.expr_node.end_pos

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
        return f"(VarDeclaration: {self.var_name_token.value}: {self.expr_node})"

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
    def __init__(self, condition_node, statement_list, start_pos, end_pos):
        self.condition_node = condition_node
        self.statement_list = statement_list

        self.start_pos = start_pos
        self.end_pos = end_pos

    def __repr__(self):
        return f'while {self.condition_node} then {self.statement_list}'

class FunctionDefNode:
    def __init__(self, name_token, arg_names, statement_list, start_pos=None):
        self.name_token = name_token
        self.arg_names = arg_names
        self.statement_list = statement_list

        self.start_pos = start_pos
        self.end_pos = self.statement_list[len(statement_list)-1].end_pos if self.statement_list else self.start_pos

    def __repr__(self):
        return f'FunctionDef: {self.name_token.value}({self.arg_names}) do {self.statement_list}'

class FunctionCallNode:
    def __init__(self, atom, args, end_pos=None):
        self.atom = atom
        self.args = args

        self.start_pos = self.atom.start_pos
        self.end_pos = end_pos

    def __repr__(self):
        return f'FunctionCall: {self.atom.value}({self.args})'

class ReturnNode:
    def __init__(self, expr_node, start_pos, end_pos):
        self.expr_node = expr_node

        self.start_pos = start_pos
        self.end_pos = self.expr_node.end_pos if expr_node else end_pos

    def __repr__(self):
        return f'Return: {self.expr_node}'

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
        self.class_name = class_name_token.value
        self.property_list = property_list

        self.start_pos = start_pos
        self.end_pos = end_pos
    
    def __repr__(self):
        return f"(ClassDef: {self.class_name} Properties: {self.property_list})"

class ClassAccessNode:
    def __init__(self, expr_node, field_token, start_pos, end_pos):
        self.expr_node = expr_node
        self.field_token = field_token

        self.start_pos = start_pos
        self.end_pos = end_pos
    
    def __repr__(self):
        return f"(ClassAccess: {self.expr_node}.{self.field_token.value})"

# class TypeCastNode:
#     def __init__(self, type_name_token) -> None:
#         self.type_name_token = type_name_token

#         self.start_pos = self.type_name_token.start_pos
#         self.end_pos = self.type_name_token.end_pos
    
#     def __repr__(self):
#         return f"(TypeCast: {self.type_name_token.value})"
