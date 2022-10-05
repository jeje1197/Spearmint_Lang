from interpreter.Classes import Function, String


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
list_args = []

# Type function implementation
def execute_create_list(interpreter, context):
    print("called")
    return list()

list_function = Function('list', list_args, statement_list=None, built_in=True)
list_function.execute = execute_create_list

# -------------------- List of built-in-functions --------------------
built_in_functions = [print_function, type_function, list_function]