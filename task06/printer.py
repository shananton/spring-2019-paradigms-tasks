import model
from textwrap import indent


def indent_statement(stmt):
    return indent(stmt, '\t')


def terminate_statement(stmt):
    if stmt.endswith('}'):
        return stmt
    else:
        return stmt + ';'


class PrettyPrinter(model.ASTNodeVisitor):
    """
        Представляет команду языка Ять в виде строки.
    """

    def visit(self, node: model.ASTNode):
        return node.accept(self)

    def visit_statement(self, node: model.ASTNode):
        return terminate_statement(self.visit(node))

    def visit_block(self, block):
        return ('{\n' +
                ''.join(indent_statement(self.visit_statement(stmt)) + '\n'
                        for stmt in block) +
                '}')

    def visit_number(self, number: model.Number):
        return str(number.value)

    def visit_function(self, function: model.Function):
        raise TypeError(
            'Functions cannot be present outside of a function definition')

    def visit_function_definition(self,
                                  fun_definition: model.FunctionDefinition):
        return 'def {name}({args}) {body}'.format(
            name=fun_definition.name,
            args=', '.join(fun_definition.function.args),
            body=self.visit_block(fun_definition.function.body))

    def visit_conditional(self, conditional: model.Conditional):
        if conditional.if_false:
            return 'if ({cond}) {if_true} else {if_false}'.format(
                cond=self.visit(conditional.condition),
                if_true=self.visit_block(conditional.if_true or []),
                if_false=self.visit_block(conditional.if_false or []))
        else:
            return 'if ({cond}) {if_true}'.format(
                cond=self.visit(conditional.condition),
                if_true=self.visit_block(conditional.if_true or []))

    def visit_print(self, print_cmd: model.Print):
        return 'print {}'.format(self.visit(print_cmd.expr))

    def visit_read(self, read_cmd: model.Read):
        return 'read {}'.format(read_cmd.name)

    def visit_function_call(self, fun_call: model.FunctionCall):
        return '{fun_expr}({args})'.format(
            fun_expr=self.visit(fun_call.fun_expr),
            args=', '.join(self.visit(expr) for expr
                           in fun_call.args)
        )

    def visit_reference(self, reference: model.Reference):
        return reference.name

    def visit_binary_operation(self, bin_operation: model.BinaryOperation):
        return '({lhs}) {op} ({rhs})'.format(
            lhs=self.visit(bin_operation.lhs),
            op=bin_operation.op,
            rhs=self.visit(bin_operation.rhs)
        )

    def visit_unary_operation(self, un_operation: model.UnaryOperation):
        return '{op}({expr})'.format(
            op=un_operation.op,
            expr=self.visit(un_operation.expr)
        )


def pretty_print(node: model.ASTNode):
    print(PrettyPrinter().visit_statement(node))
