from error.Error import RTError
from interpreter.SymbolTable import SymbolTable

class Value:
    def __init__(self, value, type):
        self.value = value
        self.type = type
        self.context = None
        self.start_pos = None
        self.end_pos = None
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
        if self.type == other.type:
            return Boolean(id(self) == id(other)).set_context(self.context).set_pos(self.start_pos, self.end_pos), None

        return None, self.illegal_operation(other)
    
    def get_comparison_ne(self, other):
        if self.type == other.type:
            return Boolean(id(self) != id(other)).set_context(self.context).set_pos(self.start_pos, self.end_pos), None
        
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
        if not other: 
            other = self
        return RTError(f'Illegal operation on {self.type}', self.start_pos, self.context)


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
            return Boolean(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)
    
    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)
    
    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def anded_by(self, other):
        if isinstance(other, Number):
            return Boolean(1 if (self.value and other.value) else 0).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def notted(self):
        return Boolean(1 if self.value == 0 else 0).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        return copy


class String(Value):
    def __init__(self, value):
        super().__init__(value, "String")

    def added_to(self, other):
        return String(self.value + str(other.value)), None

    def get_comparison_eq(self, other):
        if isinstance(other, String):
            return Boolean(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)
    
    def get_comparison_ne(self, other):
        if isinstance(other, String):
            return Boolean(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self.start_pos, other.end_pos)

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.start_pos, self.end_pos)
        copy.set_context(self.context)
        return copy

class Boolean(Number):
    def __init__(self, value=0):
        super().__init__(value)
        self.type = "Boolean"
        self.boolean_value = "true" if self.value else "false"

    def copy(self):
        return Boolean(self.value)

    def __str__(self):
        return self.boolean_value

    def __repr__(self):
        return self.__str__()


class Function(Value):
    def __init__(self, name, arg_names, statement_list, built_in=False) -> None:
        super().__init__(1, "Function")
        self.name = name
        self.arg_names = arg_names
        self.statement_list = statement_list
        self.built_in = built_in

    def check_args(self, args_passed):
        num_args_passed = len(args_passed)
        num_args_in_def = len(self.arg_names)

        if num_args_passed < num_args_in_def:
            return False, RTError(f'Function {self.name} is missing {num_args_in_def - num_args_passed} argument{"s" if num_args_in_def - num_args_passed > 1 else ""}')
            # raise Exception(
            #     f'Function {self.name} is missing {num_args_in_def - num_args_passed} argument{"s" if num_args_in_def - num_args_passed > 1 else ""}'
            #     )
        elif num_args_passed > num_args_in_def:
            return False, RTError(f'Function {self.name} has {num_args_passed - num_args_in_def} too many arguments')
            # raise Exception(f'Function {self.name} has {num_args_passed - num_args_in_def} too many arguments')
        return True, None

    def execute(self):
        raise Exception(f'{self.name}.execute method is undefined.')

    def copy(self):
        return self

    def __str__(self):
        return f'<{self.type} Name: "{self.name}", Args: {self.arg_names}, Statements: {self.statement_list}>'

    def __repr__(self):
        return self.__str__()

class List(Value):
    def __init__(self):
        super().__init__([], "List")

    def size(self):
        return Number(len(self.value))

    def add(self, object):
        self.value.append(object)

    def get(self, index):
        if (0 <= index and index < len(self.value)):
            return self.value[index]
        else:
            raise Exception(f"Insert index {index} is out of bounds")

    def insert(self, index, object):
        if (0 <= index and index < len(self.value)):
            self.value.insert(object)
        else:
            raise Exception(f"Insert index {index} is out of bounds")

    def clear(self):
        self.value.clear()

    def copy(self):
        return self

class Class(Value):
    def __init__(self, class_name):
        super().__init__(1, class_name)
        self.fields = SymbolTable()

    def add_field(self, field_name, value):
        self.fields.set(field_name, value)

    def access_field(self, field_name):
        field = self.fields.get(field_name)
        if not field:
            return self.no_property_found(field_name)
        return field

    def no_function_redefinition(self, property_name):
        raise Exception(f'{property_name} property is already defined in {self.class_name}.')

    def no_property_found(self, property_name):
        raise Exception(f'No {property_name} property defined in {self.class_name}.')

    def copy(self):
        # copy = Class(self.class_name)
        # copy.fields = self.fields
        # copy.functions = self.functions
        # return copy
        return self

    def __str__(self) -> str:
        return f'(Class Name: "{self.type}"\n\tFields: {self.fields}\n)' 

    def __repr__(self) -> str:
        return self.__str__() 

    def create_object(self):
        return Object(self.type, self.fields.copy())

class Object(Value):
    def __init__(self, type: str, fields: SymbolTable) -> None:
        self.type = type
        self.fields = fields
        self.fields.set('this', self)

    def copy(self):
        return self

    def __str__(self) -> str:
        return f'<{self.type} Object \n\tFields: {self.fields}\n>' 

    def __repr__(self) -> str:
        return self.__str__()