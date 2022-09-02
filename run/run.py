import os

from interpreter.Classes import Number
from interpreter.Context import Context
from interpreter.Interpreter import Interpreter
from interpreter.SymbolTable import SymbolTable
from lexer.Lexer import Lexer
from parser.Parser import Parser


def run(file_name, input_text, compile=False) -> str:

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

    #Interpreter
    interpreter = Interpreter(global_context)
    interpreter.add_BuiltInFunctions()
    try:
        interpreter.visit(statement_list, global_context)
    except Exception as e:
        print(e)
        return str(e)

    if interpreter.error:
        print(interpreter.error)
        return str(interpreter.error)
    else:
        return interpreter.output

def run_from_file(file_name, transpile):
    print(f'----- Reading From File: {file_name} -----')

    # Read from file
    file = open(file_name)
    file_text = file.read()
    file.close()

    # Pass in file text as input
    run(file_name, file_text, transpile)

def run_tests():
    print('Running Tests...')

    path = 'tests/'
    test_files = os.listdir(path)

    if len(test_files) == 0:
        print('No test files found. Add files to ./tests directory.')
        return

    print(f'Found files: {test_files}\n')

    for file_name in test_files:
        file_path = path + file_name
        run_from_file(file_path, transpile=False)

