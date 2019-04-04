import pytest
from model import *
from printer import *
from textwrap import dedent


def test_conditional_from_readme():
    assert (PrettyPrinter().visit(Conditional(Number(42), [], []))
            == dedent('''\
                if (42) {
                }'''))


def test_conditional_only_true():
    assert (PrettyPrinter().visit(
        Conditional(Number(42), [Number(123), Number(456)], None))
            == dedent('''\
            if (42) {
            \t123;
            \t456;
            }'''))


def test_conditional_both():
    assert (PrettyPrinter().visit(
        Conditional(Number(42), [Number(123)], [Number(456), Number(789)]))
            == dedent('''\
            if (42) {
            \t123;
            } else {
            \t456;
            \t789;
            }'''))


def test_function_definition_from_readme():
    assert (PrettyPrinter().visit(FunctionDefinition('foo', Function([], [])))
            == dedent('''\
            def foo() {
            }'''))


def test_function_definition_with_arguments_and_body():
    assert (PrettyPrinter().visit(
        FunctionDefinition('bar',
                           Function(['fst', 'snd'],
                                    [Number(42), Number(123)])))
            == dedent('''\
            def bar(fst, snd) {
            \t42;
            \t123;
            }'''))


def test_print_from_readme():
    assert (PrettyPrinter().visit(Print(Number(42))) == 'print 42;')


def test_read_from_readme():
    assert (PrettyPrinter().visit(Read('x')) == 'read x;')


def test_number_from_readme():
    assert (PrettyPrinter().visit(Number(10)) == '10;')


def test_reference_from_readme():
    assert (PrettyPrinter().visit(Reference('x')) == 'x;')


def test_binary_operation_from_readme():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    assert (PrettyPrinter().visit(mul) == '(1) * ((2) + (3));')


def test_unary_operation_from_readme():
    assert (PrettyPrinter().visit(UnaryOperation('-', Number(42))) == '-(42);')


def test_function_call_from_readme():
    assert (PrettyPrinter().visit(
        FunctionCall(Reference('foo'), [Number(1), Number(2), Number(3)]))
            == 'foo(1, 2, 3);')


def test_end_to_end_from_readme(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    expected_result = dedent('''\
    def main(arg1) {
    \tread x;
    \tprint x;
    \tif ((2) == (3)) {
    \t\tif (1) {
    \t\t}
    \t} else {
    \t\texit(-(arg1));
    \t}
    }\n''')

    assert capsys.readouterr().out == expected_result


if __name__ == '__main__':
    pytest.main()
