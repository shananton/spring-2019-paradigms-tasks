import model


class PrettyPrinter(model.ASTNodeVisitor):
    """
        Представляет команду языка Ять в виде строки.
    """

    def __init__(self):
        self.program = ''

    def get_string(self):
        return self.program

    def append(self, s):
        self.program += s

    def visit(self, node, is_statement):
        node.accept(self)
        if is_statement:
            if not self.program.endswith('}'):
                self.append(';')
            self.append('\n')

    def visit_number(self, node):
        self.append(str(node.value))

    def visit_function(self, node):
        raise NotImplementedError(
            'Functions cannot be present outside of a function definition')

    def visit_function_definition(self, node):
        self.append('def {name}({args}) {\n'.format(
            name=node.name,
            args=', '.join(node.function.args)))

        for stmt in node.function.body:
            self.visit(stmt, True)

        self.append('}\n')

    def visit_conditional(self, node):
        self.append('if (')
        self.visit(node.condition, False)
        self.append(') {\n')

        if node.if_true:
            for stmt in node.if_true:
                self.visit(stmt, True)
        if node.if_false:
            self.append('} else {\n')
            for stmt in node.if_false:
                self.visit(stmt, True)

        self.append('}')

    def visit_print(self, node):
        self.append('print ')
        self.visit(node.expr, False)

    def visit_read(self, node):
        self.append('read {name}'.format(name=node.name))

    def visit_function_call(self, node):
        self.visit(node.fun_expr, False)
        self.append('(')
        for i, expr in enumerate(node.args):
            if i > 0:
                self.append(', ')
            self.visit(expr, False)
        self.append(')')

    def visit_reference(self, node):
        self.append(node.name)

    def visit_binary_operation(self, node):
        self.append('(')
        self.visit(node.lhs, False)
        self.append(')')

        self.append(' {} '.format(node.op))

        self.append('(')
        self.visit(node.rhs, False)
        self.append(')')

    def visit_unary_operation(self, node):
        self.append(node.op)

        self.append('(')
        self.visit(node.expr, False)
        self.append(')')
