This is a compiler project for the Joseph's Programming Language (JPL)
implemented in the Python language.

The finished project will include three core features:

1) An Interpreter
2) A C Transpiler
3) A Source Compiler (after transpiling)


Shell:

JPL> *Code or command goes here*

This is the entry point for your JPL code.
You can enter a JPL expression for direct interpretation 
or use a command line tag.

    tags: 
        -r = read & interpret file
            ex: -r example.jpl

        -t = transpile 
            ex: -t example.jpl

Examples:
    JPL> 2+2
    4

    JPL> -r example.jpl

    JPL> -t example.jpl





Example Code in the JPL language:

void main() {
    a = 4
    // This is a JPL comment

    if (a == 0) {
        out("A is equal to 0")
    } else if (a % 2 == 0) {
        out("A is even")
    } else {
        out("A is odd")
    }
}# Spearmint_Lang
