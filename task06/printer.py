import model
from textwrap import dedent


class PrettyPrinter(model.ASTNodeVisitor):
    """
        Представляет команду языка Ять в виде строки.
    """

    def visit(self, node: model.ASTNode, is_statement=True):
        res = node.accept(self)
        return res + (';' if is_statement and not res.endswith('}') else '')

    def visit_number(self, number: model.Number):
        return str(number.value)

    def visit_function(self, function: model.Function):
        raise NotImplementedError(
            'Functions cannot be present outside of a function definition')

    def visit_function_definition(self,
                                  fun_definition: model.FunctionDefinition):
        return dedent('''\
        def {name}({args}) {{
            {body}
        }}\
        ''') \
            .format(
            name=fun_definition.name,
            args=', '.join(fun_definition.function.args),
            body='\n'.join(
                self.visit(stmt, True) for stmt
                in fun_definition.function.body))

    def visit_conditional(self, conditional: model.Conditional):
        return dedent('''\
        if ({cond}) {{
            {if_true}
        {else_header}   {if_false}
        }}\
        ''') \
            .format(
            cond=self.visit(conditional.condition, False),
            if_true='\n'.join(self.visit(stmt, True) for stmt
                              in (conditional.if_true or [])),
            else_header='}} else {{\n' if conditional.if_false else '',
            if_false='\n'.join(self.visit(stmt, True) for stmt
                               in (conditional.if_false or [])))

    def visit_print(self, print_cmd: model.Print):
        return 'print {expr}'.format(expr=self.visit(print_cmd.expr, False))

    def visit_read(self, read_cmd: model.Read):
        return 'read {name}'.format(name=read_cmd.name)

    def visit_function_call(self, fun_call: model.FunctionCall):
        return '{fun_expr}({args})'.format(
            fun_expr=self.visit(fun_call.fun_expr, False),
            args=', '.join(self.visit(expr, False) for expr in fun_call.args)
        )

    def visit_reference(self, reference: model.Reference):
        return reference.name

    def visit_binary_operation(self, bin_operation: model.BinaryOperation):
        return '({lhs}) {op} ({rhs})'.format(
            lhs=self.visit(bin_operation.lhs, False),
            op=bin_operation.op,
            rhs=self.visit(bin_operation.rhs, False)
        )

    def visit_unary_operation(self, un_operation: model.UnaryOperation):
        return '{op}({expr})'.format(
            op=un_operation.op,
            expr=self.visit(un_operation.expr, False)
        )


def pretty_print(node: model.ASTNode):
    print(PrettyPrinter().visit(node))
