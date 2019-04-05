import model
from textwrap import indent


def indent_statement(stmt):
    return indent(stmt, '\t') + '\n'


def format_statement(stmt):
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

    def apply(self, node: model.ASTNode, *, is_statement=True):
        res = self.visit(node)
        return format_statement(res) if is_statement else res

    def visit_number(self, number: model.Number):
        return str(number.value)

    def visit_function(self, function: model.Function):
        raise NotImplementedError(
            'Functions cannot be present outside of a function definition')

    def visit_function_definition(self,
                                  fun_definition: model.FunctionDefinition):
        return 'def {name}({args}) {{\n{body}}}'.format(
            name=fun_definition.name,
            args=', '.join(fun_definition.function.args),
            body=''.join(
                indent_statement(self.apply(stmt)) for stmt
                in fun_definition.function.body))

    def visit_conditional(self, conditional: model.Conditional):
        return 'if ({cond}) {{\n{if_true}{else_header}{if_false}}}'.format(
            cond=self.apply(conditional.condition, is_statement=False),
            if_true=''.join(indent_statement(self.apply(stmt)) for stmt
                            in (conditional.if_true or [])),
            else_header='} else {\n' if conditional.if_false else '',
            if_false=''.join(indent_statement(self.apply(stmt)) for stmt
                             in (conditional.if_false or [])))

    def visit_print(self, print_cmd: model.Print):
        return 'print {expr}' \
            .format(expr=self.apply(print_cmd.expr, is_statement=False))

    def visit_read(self, read_cmd: model.Read):
        return 'read {name}'.format(name=read_cmd.name)

    def visit_function_call(self, fun_call: model.FunctionCall):
        return '{fun_expr}({args})'.format(
            fun_expr=self.apply(fun_call.fun_expr, is_statement=False),
            args=', '.join(self.apply(expr, is_statement=False) for expr
                           in fun_call.args)
        )

    def visit_reference(self, reference: model.Reference):
        return reference.name

    def visit_binary_operation(self, bin_operation: model.BinaryOperation):
        return '({lhs}) {op} ({rhs})'.format(
            lhs=self.apply(bin_operation.lhs, is_statement=False),
            op=bin_operation.op,
            rhs=self.apply(bin_operation.rhs, is_statement=False)
        )

    def visit_unary_operation(self, un_operation: model.UnaryOperation):
        return '{op}({expr})'.format(
            op=un_operation.op,
            expr=self.apply(un_operation.expr, is_statement=False)
        )


def pretty_print(node: model.ASTNode):
    print(PrettyPrinter().apply(node))
