# SECD Machine project
# Python 2.x implementation

import sys
import argparse


def secd_isalnum(c):
    return c.isalnum()


# Parse input string into a list of all parentheses and atoms (int or str),
# exclude whitespaces.
def normalize_str(str):
    str_norm = []
    last_c = None
    for c in str:
        if secd_isalnum(c):
            if secd_isalnum(last_c):
                str_norm[-1] += c
            else:
                str_norm.append(c)
        elif not c.isspace():
            str_norm.append(c)
        last_c = c
    return str_norm


# Generate abstract syntax tree from normalized input.
def ast_get(input_norm):
    ast = []
    # Go through each element in the input:
    # - if it is an open parenthesis, find matching parenthesis and make recursive
    #   call for content in-between. Add the result as an element to the current list.
    # - if it is an atom, just add it to the current list.
    i = 0
    while i < len(input_norm):
        symbol = input_norm[i]
        if symbol == '(':
            list_content = []
            match_ctr = 1  # If 0, parenthesis has been matched.
            while match_ctr != 0:
                i += 1
                if i >= len(input_norm):
                    raise ValueError("Invalid input: Unmatched open parenthesis.")
                symbol = input_norm[i]
                if symbol == '(':
                    match_ctr += 1
                elif symbol == ')':
                    match_ctr -= 1
                if match_ctr != 0:
                    list_content.append(symbol)
            ast.append(ast_get(list_content))
        elif symbol == ')':
            raise ValueError("Invalid input: Unmatched close parenthesis.")
        else:
            try:
                ast.append(int(symbol))
            except ValueError:
                ast.append(symbol)
        i += 1
    return ast


secd_pycomp_primitives = {
    "car": lambda expression, n, controls: secd_pycomp(expression[1], n, ['CAR'] + controls),
    "cdr": lambda expression, n, controls: secd_pycomp(expression[1], n, ['CDR'] + controls),
    "atom": lambda expression, n, controls: secd_pycomp(expression[1], n, ['ATOM'] + controls),
    "quote": lambda expression, n, controls: ['LDC', expression[1]] + controls,
    "cons": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                        secd_pycomp(expression[1], n, ['CONS'] + controls)),
    "eq": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                      secd_pycomp(expression[1], n, ['EQ'] + controls)),
    "leq": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                       secd_pycomp(expression[1], n, ['LEQ'] + controls)),
    "add": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                       secd_pycomp(expression[1], n, ['ADD'] + controls)),
    "sub": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                       secd_pycomp(expression[1], n, ['SUB'] + controls)),
    "mul": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                       secd_pycomp(expression[1], n, ['MUL'] + controls)),
    "div": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                       secd_pycomp(expression[1], n, ['DIV'] + controls)),
    "rem": lambda expression, n, controls: secd_pycomp(expression[2], n,
                                                       secd_pycomp(expression[1], n, ['REM'] + controls)),
    "if": lambda expression, n, controls: secd_pycomp(expression[1], n, ['SEL'] +
                                                      [secd_pycomp(expression[2], n, ['JOIN'])] +
                                                      [secd_pycomp(expression[3], n, ['JOIN'])] + controls),
    "lambda": lambda expression, n, controls: ['LDF'] + [
        [expression[1]] + secd_pycomp(expression[2], n, ['RTN'])] + controls,
    "let": lambda expression, n, controls: ['DUM'] +
                                           secd_pycomplist(expression[1], n,
                                                           ['LDF'] +
                                                           [[secd_pycompvars(expression[1])] +
                                                            secd_pycomp(expression[2], n, ['RTN'])]) +
                                           ['RAP'] +
                                           controls
}


def secd_pycomplist(elist, n, controls):
    if 0 != len(elist):
        controls = secd_pycomplist(elist[1:], n, secd_pycomp(elist[0][1], n, controls))
    return controls


def secd_pycompvars(elist):
    ret = []
    if 0 != len(elist):
        ret = [elist[0][0]] + secd_pycompvars(elist[1:])
    return ret


def secd_pycompargs(elist, n, controls):
    if 0 != len(elist):
        controls = secd_pycompargs(elist[1:], n, secd_pycomp(elist[0], n, controls))
    return controls


def secd_pycomp(expression, n, controls):
    """
    Compiles a funx expression presented as an AST to a SECD control list
    :param expression: funx source AST
    :param n: namespace (not used)
    :param controls: control list built incrementally
    :return: control list
    """
    if str == type(expression):
        controls = ['LD', expression] + controls
    else:
        if str == type(expression[0]) and None != secd_pycomp_primitives.get(expression[0]):
            # A funx primitive
            controls = secd_pycomp_primitives[expression[0]](expression, n, controls)
        else:
            # This must be an application of a user-defined function
            controls = ['SKP'] + secd_pycompargs(expression[1:], n, secd_pycomp(expression[0], n, ['AP'] + controls))
    return controls


def ast_put(ast,s):
    if 0==len(ast):
        return s
    else:
        if str==type(ast[0]) or int==type(ast[0]):
            s = ast_put( ast[1:], "%s %s" % (s,ast[0]))
        else:
            srest = ast_put( ast[0], "")
            s = ast_put( ast[1:], "%s (%s)" % (s,srest[1:]))
        return s



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='funx Python Strict Compiler.\n(c) jmc 2021.')
    parser.add_argument('--log', type=argparse.FileType('w'), help='logfile (optional)')
    parser.add_argument('--verbose', default=False, action='store_true')
    # parser.add_argument('--alternate', default='compiler.secd', help='alternate compiler secd-file')
    parser.add_argument('infile', type=argparse.FileType('r'), help='funx source file')
    parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'),
                        default=sys.stdout, help='secd compiled file')
    args = parser.parse_args()

    funx_source = args.infile.read()
    str_norm = normalize_str(funx_source)
    ast = ast_get(str_norm)[0]
    # print(str_norm)
    # print(ast)
    pycontrols = secd_pycomp(ast, None, ['STOP'])
    controls =ast_put(pycontrols,"")

    args.outfile.write("(%s)" % controls[1:])
    if None != args.log:
        args.log.close()
    args.infile.close()
    args.outfile.close()
