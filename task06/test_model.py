#!/usr/bin/env python3
import sys

import pytest
from model import *
from io import StringIO


def test_scope_in_dict():
    a = object()
    scope = Scope()
    scope['foo'] = a
    assert scope['foo'] is a


def test_scope_only_in_parent():
    a, b = object(), object()
    parent = Scope()
    child = Scope(parent)
    parent['foo'] = a
    child['bar'] = b
    assert child['foo'] is a
    assert child['bar'] is b


def test_scope_pick_closest():
    a, b, c, d, e, f = [object() for _ in range(6)]
    assert a is not b
    grandparent = Scope()
    parent = Scope(grandparent)
    child1, child2 = Scope(parent), Scope(parent)
    grandparent['foo'] = a
    parent['foo'] = b
    child1['foo'] = c
    child2['foo'] = d
    parent['bar'] = e
    grandparent['baz'] = f
    assert child1['foo'] is c
    assert child2['foo'] is d
    assert child1['bar'] is e
    assert parent['foo'] is b
    assert child1['baz'] is f


def test_scope_not_found():
    a, b = object(), object()
    parent = Scope()
    child = Scope(parent)
    parent['foo'] = a
    child['bar'] = b
    with pytest.raises(KeyError, match='baz'):
        _ = child['baz']


def test_function_definition():
    scope = Scope()
    fun = Function(['x'], [Number(1)])
    fun_def = FunctionDefinition('const1', fun)
    fun_def.evaluate(scope)
    assert scope['const1'] == fun


def test_conditional_true():
    scope = Scope()
    cond_true = Conditional(Number(1), [Number(10)], [Number(20)])
    assert cond_true.evaluate(scope) == Number(10)


def test_conditional_false():
    scope = Scope()
    cond_false = Conditional(Number(0), [Number(10)], [Number(20)])
    assert cond_false.evaluate(scope) == Number(20)


def test_conditional_false_partial():
    scope = Scope()
    cond_partial_false = Conditional(Number(0), [Number(10)])
    assert cond_partial_false.evaluate(scope) is None


def test_print(capsys):
    scope = Scope()
    print_expr = Print(Number(13))
    assert print_expr.evaluate(scope) == Number(13)
    assert capsys.readouterr().out == '13\n'


def test_read(monkeypatch):
    monkeypatch.setattr(sys, 'stdin', StringIO('1279'))
    scope = Scope()
    read_expr = Read('foo')
    assert read_expr.evaluate(scope) == Number(1279)
    assert scope['foo'] == Number(1279)


def test_function_call():
    scope = Scope()
    fun = Function(['x'], [Reference('x')])
    fun_call = FunctionCall(fun, [Number(9)])
    assert fun_call.evaluate(scope) == Number(9)


def test_reference():
    scope = Scope()
    scope['foo'] = Number(6)
    ref = Reference('foo')
    assert ref.evaluate(scope) == Number(6)


def test_binary_operation():
    scope = Scope()
    bin_op = BinaryOperation(Number(17), '+', Number(25))
    assert bin_op.evaluate(scope) == Number(42)


def test_binary_division_rounding_down():
    scope = Scope()
    bin_div_rounding_down = BinaryOperation(Number(-3), '/', Number(2))
    assert bin_div_rounding_down.evaluate(scope) == Number(-2)


def test_binary_and_logical_instead_of_bitwise():
    scope = Scope()
    bin_and = BinaryOperation(Number(1), '&&', Number(2))
    assert bin_and.evaluate(scope) != Number(0)


def test_binary_operation_not_in_dict():
    with pytest.raises(KeyError, match='>>='):
        _ = BinaryOperation(Number(1), '>>=', Number(2))


def test_unary_operation():
    scope = Scope()
    un_op = UnaryOperation('-', Number(3))
    assert un_op.evaluate(scope) == Number(-3)


def test_unary_operation_not_in_dict():
    with pytest.raises(KeyError, match='~'):
        _ = UnaryOperation('~', Number(1))


def test_factorial():
    factorial_fun = Function(['n'], [
        Conditional(BinaryOperation(Reference('n'), '<=', Number(1)),
                    [
                        Number(1)
                    ],
                    [
                        BinaryOperation(
                            Reference('n'),
                            '*',
                            FunctionCall(
                                Reference('fac'),
                                [BinaryOperation(Reference('n'),
                                                 '-',
                                                 Number(1))]
                            )
                        )
                    ])
    ])
    factorial_def = FunctionDefinition('fac', factorial_fun)
    scope = Scope()
    factorial_def.evaluate(scope)
    assert FunctionCall(Reference('fac'),
                        [Number(5)]).evaluate(scope) == Number(120)


def test_construction():
    FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    Read('n')
    Print(
        UnaryOperation('-', FunctionCall(Reference('fac'), [Reference('n')]))
    )


if __name__ == "__main__":
    pytest.main()
