class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    # Looks for key in local symbol table. If not found, checks parents for key.
    # If neither are found, returns None
    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def get_locally(self, name):
        return self.symbols.get(name, None)

    # Returns the value and the lowest context containing the key
    def get_with_lowest_context(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get_with_lowest_context(name)
        return value, self

    # Sets the key-value pair in the lowest context containing the key.
    # If no contexts contain the key, sets the key-value pair in the current context.
    def set(self, name, value):
        # Check if defined in scope
        value_in_lowest_context, symbol_table_of_lowest_context = self.get_with_lowest_context(name)
        if value_in_lowest_context:
            symbol_table_of_lowest_context.symbols[name] = value
            return True # If setting value of higher context
        else:
            self.symbols[name] = value
            return False # If setting in local context

    def set_locally(self, name, value):
        self.symbols[name] = value

    def set_multiple(self, list):
        for name, value in list:
            self.symbols[name] = value

    # Removes the key-value pair from the current context
    def remove(self, name):
        del self.symbols[name]

    def __repr__(self) -> str:
        return str(self.symbols)