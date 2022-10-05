# This file is the entry point for the JPL transpiler.
# Running this file creates a shell and prompts the user
# for input. Input is then passed to the run function.

# Imports
from run.run import run, run_from_file

def command_prompt():
    # print("Starting JPL...")

    while True:
        should_transpile = False
        # Prompt input from the user and strip leading/trailing spaces
        user_input = input("Spearmint>").strip()

        # Ignore empty inputs
        if user_input == "":
            continue

        # Break
        commands = user_input.split(' ')

        
        # Exit shell with '-q' tag
        if '--q' in commands:
            break

        # Read directly from file argument with '-r' tag
        if '--r' in commands:
            index = 1
            while index < len(commands) and '-' in commands[index]:
                index += 1

            if index >= len(commands):
                print('No file argument found. Enter a file name. Ex: file_name.jpl')
                continue

            # if not '.spm' in commands[index]:
            #     commands[index]

            run_from_file(commands[index])
        else:
            # Run interpreter/transpiler
            run("Command Line", user_input)

command_prompt()
