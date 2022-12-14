from ast import FunctionDef
from error.Error import RTError
from interpreter.Classes import Class, Function, Number, String
from interpreter.Context import Context
from interpreter.SymbolTable import SymbolTable
from lexer.Token import Token
from interpreter.BuiltInFunctions import built_in_functions
from parser.Nodes import FunctionDefNode, VarDeclarationNode

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
        res_list = []
        for node in statement_list:
            res = self.visit(node, context)
            if self.error: return

            res_list.append(res)
            if self.should_return or self.should_break or self.should_continue: 
                break
        return res_list

    # Performs an operation on one nodes
    def visit_UnaryOpNode(self, node, context):
        expr = self.visit(node.expr_node, context)
        if self.error: return

        result, error = None, None
        if node.op_token.type == Token.MINUS:
            result, error = expr.multed_by(Number(-1))
        elif node.op_token.type == Token.NOT:
            result, error = expr.notted()

        if error:
            self.error = error
            return
        return result

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

        if error:
            error.position = node.op_token.start_pos
            self.error = error
            return

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
        return value

    # Assigns a variable a value
    def visit_VarAssignNode(self, node, context):
        var_name = node.var_name_token.value
        if context.symbol_table.get(var_name) is None:
            self.error = RTError(f"{var_name} hasn't been declared yet.", node.var_name_token.start_pos)
            return

        value = self.visit(node.value_node, context)
        if self.error: return

        context.symbol_table.set(var_name, value)
        return value

    # Accesses variable if defined in scope of JPL code
    def visit_VarAccessNode(self, node, context):
        var_name = node.var_name_token.value
        value = None

        if context.symbol_table.get(var_name):
            value = context.symbol_table.get(var_name)
        elif self.function_symbol_table.get(var_name):
            value = self.function_symbol_table.get(var_name)
        else:
            print("here")
            self.error = RTError(f"{var_name} is undefined", node.start_pos, context)
            return
        return value.copy().set_context(context)

    # Runs an if statement from an IfNode
    def visit_IfNode(self, node, context):
        # res = None
        for condition, statement_list in node.cases:
            condition_value = self.visit(condition, context)
            if self.error: return

            if condition_value.is_true():
                new_context = self.generate_new_context("if statement", context)
                self.visit(statement_list, new_context)
                if self.error or self.should_return: return

        if node.else_statement_list:
            self.visit(node.else_statement_list, context)

    # Runs a for loop from a ForNode
    def visit_ForNode(self, node, context):
        condition_context = self.generate_new_context(f'For Loop (Condition)', context)
        if node.init_statement:
            self.visit(node.init_statement, condition_context)
            if self.error: return

        # Check condition
        condition = self.visit(node.condition_node, condition_context)
        if self.error: return

        while condition.is_true():
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

            if node.update_statement:
                self.visit(node.update_statement, condition_context)

            # Check condition
            condition = self.visit(node.condition_node, condition_context)
            if self.error: return
            

    # Runs a while loop from a WhileNode
    def visit_WhileNode(self, node, context):

        # Check condition
        condition = self.visit(node.condition_node, context)
        if self.error: return

        while condition.is_true():
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

            # Check condition
            condition = self.visit(node.condition_node, context)
            if self.error: return

    # Add new Function to function symbol table
    def visit_FunctionDefNode(self, node, context):
        function_name = node.name_token.value
        if context.symbol_table.get(function_name):
            self.error = RTError(f"Function '{function_name}' is already defined", node.start_pos, context)
            return

        function = Function(function_name, node.arg_names, node.statement_list)
        context.symbol_table.set(function_name, function)
        return function


    # Checks function symbol table for valid FunctionDef then executes
    def visit_FunctionCallNode(self, node, context):
        function = self.visit(node.atom, context)
        if self.error: return

        # Check that atom is a function
        if not isinstance(function, Function):
            self.error = RTError(f"{function.value} is not a function", node.start_pos, context)
            return

        # Check for same number of args as defined in function
        valid_args, error = function.check_args(node.args)
        if not valid_args:
            error.position = node.start_pos
            self.error = error
            return

        # Add args to new context
        new_context = self.generate_new_context(f'Function: {function.name}()', context)

        # Map argument variables to values passed in
        for i in range(len(function.arg_names)):
            value = self.visit(node.args[i], context)
            if self.error: return
            if value is None:
                self.error = RTError(f"Function {function.name} argument '{function.arg_names[i]}' did not receive a value", node.args[i].start_pos,
                context)
                return
            new_context.symbol_table.set_locally(function.arg_names[i], value)

        # If the function is in the standard library, call its execute method
        if function.built_in:
            return function.execute(self, new_context)
             
        # If the function is user-defined, run its body
        self.visit(function.statement_list, new_context)
        if self.error: return
        if self.should_return:
            return_val = self.return_value 
            self.should_return = False
            self.return_value = None
            return return_val

    def visit_ReturnNode(self, node, context):
        if node.expr_node is not None:
            self.return_value = self.visit(node.expr_node, context)
        self.should_return = True
        return self.return_value

    def visit_BreakNode(self, node, context):
        self.should_break = True

    def visit_ContinueNode(self, node, context):
        self.should_continue = True

    def visit_ClassDefNode(self, node, context):
        if context.symbol_table.get(node.class_name):
            raise Exception(f"Class {node.class_name} is already defined")

        class_def = Class(node.class_name)
        class_context = self.generate_new_context(f"{node.class_name}", context)

        for property in node.property_list:
            if isinstance(property, VarDeclarationNode):
                value = self.visit(property.expr_node, class_context)
                if self.error: return

                class_def.add_field(property.var_name_token.value, value)
            elif isinstance(property, FunctionDefNode):
                class_def.add_field(property.name_token.value, Function(
                    property.name_token.value, 
                    property.arg_names, 
                    property.statement_list
                ))
            else:
                raise Exception(f"{type(property)} cannot be used in class definition: {node.class_name}")

        context.symbol_table.set(node.class_name, class_def)

    def visit_ClassAccessNode(self, node, context):
        left = self.visit(node.expr_node, context)
        if self.error: return

        field = left.fields.get(node.field_name)

        if field is None:
            self.error = RTError(f"{left} does not have a '{node.field_name}' field", node.start_pos, context)
            return 
        
        return field
        
