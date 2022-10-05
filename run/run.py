import os

from interpreter.Classes import Boolean
from interpreter.Context import Context
from interpreter.Interpreter import Interpreter
from interpreter.SymbolTable import SymbolTable
from lexer.Lexer import Lexer
from parser.Parser import Parser

global_symbol_table = SymbolTable()
global_symbol_table.set_multiple([
    ("true", Boolean(1)),
    ("false", Boolean(0))
])

def run(file_name, input_text) -> str:

    # Lexer
    lexer = Lexer(file_name, input_text)
    token_list, error = lexer.get_tokens()

    if error: 
        print(error)
        return str(error)
    # else: 
    #     print(f'List of Tokens: {token_list}')


    # Parser
    parser = Parser(token_list)
    statement_list, error = parser.parse()

    if error: 
        print(error)
        return str(error)
    # else:
    #     print(f'----- Statements: {len(statement_list)} -----')
    #     for statement in statement_list:
    #         print(statement)
    #     print('----------')

    # Set context
    global_context = Context(file_name)
    global_context.set_symbol_table(global_symbol_table)

    #Interpreter
    interpreter = Interpreter(global_context)
    interpreter.add_BuiltInFunctions()
    output = None

    # try:
    output = interpreter.visit(statement_list, global_context)
    # except Exception as e:
    #     print(e)
    #     return str(e)

    if interpreter.error:
        print(interpreter.error)
        return str(interpreter.error)
    else:
        print(output)
        return interpreter.output

def run_from_file(file_name, transpile=False):
    print(f'----- Reading From File: {file_name} -----')

    # Read from file
    file = open(file_name)
    file_text = file.read()
    file.close()

    # Pass in file text as input
    run(file_name, file_text)