from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit(self, node: ASTNode):
        return node.accept(self)

    def visit_number(self, number: Number):
        return Number(number.value)

    def visit_function(self, function: Function):
        return Function(function.args.copy(),
                        list(map(self.visit, function.body)))

    def visit_function_definition(self, fun_definition: FunctionDefinition):
        return FunctionDefinition(fun_definition.name,
                                  self.visit(fun_definition.function))

    def visit_conditional(self, conditional: Conditional):
        return Conditional(self.visit(conditional.condition),
                           list(map(self.visit, conditional.if_true or [])),
                           list(map(self.visit, conditional.if_false or [])))

    def visit_print(self, print_cmd: Print):
        return Print(self.visit(print_cmd.expr))

    def visit_read(self, read_cmd: Read):
        return Read(read_cmd.name)

    def visit_function_call(self, fun_call: FunctionCall):
        return FunctionCall(self.visit(fun_call.fun_expr),
                            list(map(self.visit, fun_call.args)))

    def visit_reference(self, reference: Reference):
        return Reference(reference.name)

    def visit_binary_operation(self, bin_operation: BinaryOperation):
        res = BinaryOperation(self.visit(bin_operation.lhs),
                              bin_operation.op,
                              self.visit(bin_operation.rhs))
        if isinstance(res.lhs, Number) and isinstance(res.rhs, Number):
            return res.evaluate(Scope())

        if (res.op == '*' and
                (res.lhs == Number(0) and isinstance(res.rhs, Reference) or
                 res.rhs == Number(0) and isinstance(res.lhs, Reference))):
            return Number(0)
        if (res.op == '-' and
                isinstance(res.lhs, Reference) and
                isinstance(res.rhs, Reference) and
                res.lhs.name == res.rhs.name):
            return Number(0)
        return res

    def visit_unary_operation(self, un_operation: UnaryOperation):
        res = UnaryOperation(un_operation.op, self.visit(un_operation.expr))
        if isinstance(res.expr, Number):
            return res.evaluate(Scope())
        return res
