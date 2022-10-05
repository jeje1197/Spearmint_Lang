from interpreter.Classes import Class, Function, List, String


# -------------------- Print method --------------------
print_args = ['text']

# Print function implementation
def execute_print(interpreter, context):
    if interpreter.output:
        interpreter.output += "\n"
    value = str(context.symbol_table.get('text'))
    print(value)

print_function = Function('print', print_args, statement_list=None, built_in=True)
print_function.execute = execute_print

# -------------------- Type method --------------------
type_args = ['object']

# Type function implementation
def execute_type(interpreter, context):
    type_name = context.symbol_table.get('object').type
    return String(type_name)

type_function = Function('type', type_args, statement_list=None, built_in=True)
type_function.execute = execute_type

# -------------------- List method --------------------
new_obj_args = ['class_name']

# Object Instantiator
def execute_create_new_obj(interpreter, context):
    obj_type = context.symbol_table.get('class_name').value
    class_def = context.symbol_table.get(obj_type)
    if not isinstance(class_def, Class):
        print(type(class_def))
        raise Exception(f"Expected class name as argument")

    return class_def.create_object()

create_new_obj_function = Function('new', new_obj_args, statement_list=None, built_in=True)
create_new_obj_function.execute = execute_create_new_obj

# -------------------- List method --------------------
list_args = []

# List Instantiator
def execute_create_list(interpreter, context):
    return List()

list_function = Function('list', list_args, statement_list=None, built_in=True)
list_function.execute = execute_create_list

# -------------------- List of built-in-functions --------------------
built_in_functions = [print_function, type_function, create_new_obj_function, list_function]