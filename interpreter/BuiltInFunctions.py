from interpreter.Classes import Function

# Print method
print_args = ['text']

# Print function implementation
def execute_print(interpreter, context):
    value = str(context.symbol_table.get('text'))
    print(value)
    interpreter.output += value + "\n"

print_function = Function('print', print_args, statement_list=None, built_in=True)
print_function.execute = execute_print

built_in_functions = [print_function]