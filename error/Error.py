class Error:
    def __init__(self, message, position=None):
        self.message = message
        self.position = position

    # Returns a formatted string with an arrow pointing to
    # the place where an error occurs in the code
    def get_text_with_arrow(self):
        text = self.position.ftxt
        ln = self.position.ln
        col = self.position.col

        # Get the line with the error on it
        line_text = text.split('\n')[ln]

        # Point an arrow to where the error occurs
        arrow_str = ''
        while len(arrow_str) < col:
            arrow_str += ' '
        arrow_str += '^'

        return f'\t{line_text}\n\t{arrow_str}\n'


    def __str__(self) -> str:
        error_str = f'{self.message}\n\n'
        error_str += self.get_text_with_arrow()
        error_str += f'{self.position}\n'

        return error_str

    def __repr__(self) -> str:
        return self.__str__()

def is_error(object):
    return isinstance(object, Error)

class IllegalCharacterError(Error):
    def __init__(self, char, position=None):
        super().__init__(f"Illegal Character: '{char}'", position)
    
class InvalidSyntaxError(Error):
    def __init__(self, message, position):
        super().__init__(f'Invalid Syntax Error: {message}', position)

class RTError(Error):
    def __init__(self, message = None, position=None, context=None):
        super().__init__(f'Runtime Error: {message}', position)
        self.context = context

    def __repr__(self) -> str:
        return self.__str__()
    
    def __str__(self) -> str:
        string = self.generate_traceback()
        string += self.get_text_with_arrow()
        string += self.message
        return string

    def generate_traceback(self):
        result = ''
        pos = self.position
        ctx = self.context

        while ctx:
            result += f'\tFile <{pos.fn}>, line {str(pos.ln+1)}, col {str(pos.col+1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return f'Traceback (most recent callback):\n{result}'