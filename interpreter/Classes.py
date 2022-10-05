from error.Error import RTError
from interpreter.SymbolTable import SymbolTable

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


class Function(Value):
    def __init__(self, name, arg_names, statement_list, built_in=False) -> None:
        super().__init__(1, "Function")
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
                f'Function {self.name} is missing {num_args_in_def - num_args_passed} argument{"s" if num_args_in_def - num_args_passed > 1 else ""}'
                )
        elif num_args_passed > num_args_in_def:
            raise Exception(f'Function {self.name} has {num_args_passed - num_args_in_def} too many arguments')
        return True

    def execute(self):
        raise Exception(f'{self.name}.execute method is undefined.')

    def copy(self):
        return self

    def __str__(self):
        return f'<{self.type} Name: "{self.name}", Args: {self.arg_names}, Statements: {self.statement_list}>'

    def __repr__(self):
        return self.__str__()


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