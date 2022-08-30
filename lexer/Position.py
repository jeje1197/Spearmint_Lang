class Position:
    def __init__(self, idx, ln, col, file_name=None, file_text=None):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = file_name
        self.ftxt = file_text
    
    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0
        
        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

    def __repr__(self) -> str:
        return f'File: <{self.fn}>, line: {self.ln+1} col: {self.col+1}'