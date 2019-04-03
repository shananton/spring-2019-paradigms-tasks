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


if __name__ == '__main__':
    pytest.main()
