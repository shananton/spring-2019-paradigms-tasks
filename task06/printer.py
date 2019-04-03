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

    def visit(self, node: model.ASTNode, is_statement):
        node.accept(self)
        if is_statement:
            if not self.program.endswith('}'):
                self.append(';')
            self.append('\n')

    def visit_number(self, number: model.Number):
        self.append(str(number.value))

    def visit_function(self, function: model.Function):
        raise NotImplementedError(
            'Functions cannot be present outside of a function definition')

    def visit_function_definition(self, fun_definition: model.FunctionDefinition):
        self.append('def {name}({args}) {\n'.format(
            name=fun_definition.name,
            args=', '.join(fun_definition.function.args)))

        for stmt in fun_definition.function.body:
            self.visit(stmt, True)

        self.append('}\n')

    def visit_conditional(self, conditional: model.Conditional):
        self.append('if (')
        self.visit(conditional.condition, False)
        self.append(') {\n')

        if conditional.if_true:
            for stmt in conditional.if_true:
                self.visit(stmt, True)
        if conditional.if_false:
            self.append('} else {\n')
            for stmt in conditional.if_false:
                self.visit(stmt, True)

        self.append('}')

    def visit_print(self, print_cmd: model.Print):
        self.append('print ')
        self.visit(print_cmd.expr, False)

    def visit_read(self, read_cmd: model.Read):
        self.append('read {name}'.format(name=read_cmd.name))

    def visit_function_call(self, fun_call: model.FunctionCall):
        self.visit(fun_call.fun_expr, False)
        self.append('(')
        for i, expr in enumerate(fun_call.args):
            if i > 0:
                self.append(', ')
            self.visit(expr, False)
        self.append(')')

    def visit_reference(self, reference: model.Reference):
        self.append(reference.name)

    def visit_binary_operation(self, bin_operation: model.BinaryOperation):
        self.append('(')
        self.visit(bin_operation.lhs, False)
        self.append(')')

        self.append(' {} '.format(bin_operation.op))

        self.append('(')
        self.visit(bin_operation.rhs, False)
        self.append(')')

    def visit_unary_operation(self, un_operation: model.UnaryOperation):
        self.append(un_operation.op)

        self.append('(')
        self.visit(un_operation.expr, False)
        self.append(')')
