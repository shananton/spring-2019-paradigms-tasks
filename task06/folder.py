import model as md


class ConstantFolder(md.ASTNodeVisitor):
    def visit(self, node: md.ASTNode):
        return node.accept(self)

    def visit_number(self, number: md.Number):
        return md.Number(number.value)

    def visit_function(self, function: md.Function):
        return md.Function(function.args.copy(),
                           list(map(self.visit, function.body)))

    def visit_function_definition(self, fun_definition: md.FunctionDefinition):
        return md.FunctionDefinition(fun_definition.name,
                                     self.visit(fun_definition.function))

    def visit_conditional(self, conditional: md.Conditional):
        return md.Conditional(
            self.visit(conditional.condition),
            list(map(self.visit, conditional.if_true or [])),
            list(map(self.visit, conditional.if_false or [])))

    def visit_print(self, print_cmd: md.Print):
        return md.Print(self.visit(print_cmd.expr))

    def visit_read(self, read_cmd: md.Read):
        return md.Read(read_cmd.name)

    def visit_function_call(self, fun_call: md.FunctionCall):
        return md.FunctionCall(self.visit(fun_call.fun_expr),
                               list(map(self.visit, fun_call.args)))

    def visit_reference(self, reference: md.Reference):
        return md.Reference(reference.name)

    def visit_binary_operation(self, bin_operation: md.BinaryOperation):
        res = md.BinaryOperation(self.visit(bin_operation.lhs),
                                 bin_operation.op,
                                 self.visit(bin_operation.rhs))

        if isinstance(res.lhs, md.Number) and isinstance(res.rhs, md.Number):
            return res.evaluate(md.Scope())

        if res.op == '*':
            for arg1, arg2 in ((res.lhs, res.rhs), (res.rhs, res.lhs)):
                if (isinstance(arg1, md.Number) and
                        arg1 == md.Number(0) and
                        isinstance(arg2, md.Reference)):
                    return md.Number(0)

        if (res.op == '-' and
                isinstance(res.lhs, md.Reference) and
                isinstance(res.rhs, md.Reference) and
                res.lhs.name == res.rhs.name):
            return md.Number(0)

        return res

    def visit_unary_operation(self, un_operation: md.UnaryOperation):
        res = md.UnaryOperation(un_operation.op, self.visit(un_operation.expr))
        if isinstance(res.expr, md.Number):
            return res.evaluate(md.Scope())
        return res


def fold_constants(program):
    return ConstantFolder().visit(program)
