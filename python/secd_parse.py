# SECD Machine project
# Python 2.x implementation

import secd_mem as mem

# Parsing, graphing and printing funx sexps or cell memory trees

def secd_mem_pp(idx):
    """
    Converts memory cells to printable sexp.
    :param idx: index in cell memory
    :return: sexp pointed representation of memcells
    """
    if mem.g_mem[idx]['car'] == 0 and mem.g_mem[idx]['cdr'] == 0 :
        return "nil"
    else:
        s = "("
        if mem.g_mem[idx]['car'] < 0 :
            s += mem.g_symtable[-mem.g_mem[idx]['car']]
        else:
            s += secd_mem_pp(mem.g_mem[idx]['car'])
        s += ' . '
        if mem.g_mem[idx]['cdr'] < 0:
            s += mem.g_symtable[-mem.g_mem[idx]['cdr']]
        else:
            s += secd_mem_pp(mem.g_mem[idx]['cdr'])
        s += ")"
        return s


def secd_mem2dot( idx ):
    def mem2dot( idx ):

        s = 'N%d [label="<car> %s | <cdr> %s "];\n' % \
            (idx,
             mem.g_symtable[-mem.g_mem[idx]['car']] if mem.g_mem[idx]['car'] < 0 else '*',
             mem.g_symtable[-mem.g_mem[idx]['cdr']] if mem.g_mem[idx]['cdr'] <= 0 else '*')
        if mem.g_mem[idx]['cdr'] > 0:
            s += 'N%d:cdr -> N%d:car;\n' % (idx, mem.g_mem[idx]['cdr'])
            s += mem2dot(mem.g_mem[idx]['cdr'])
        if mem.g_mem[idx]['car'] > 0:
            s += 'N%d:car -> N%d:car;\n' % (idx, mem.g_mem[idx]['car'])
            s += mem2dot(mem.g_mem[idx]['car'])

        return s

    s = 'digraph cells {\nnode [shape=record];\n'
    s += mem2dot( idx )
    s += '}\n'
    return s


def secd_mem2sexp( idx, NESTED = [] ):
    if 0 != idx and NESTED.count(idx) > 1 :
        return " [...]"

    if mem.g_mem[idx]['car'] == 0 and mem.g_mem[idx]['cdr'] == 0:
        return ""
    else:
        s = "("
        if mem.g_mem[idx]['car'] <= 0:
            s += mem.g_symtable[-mem.g_mem[idx]['car']]
        else:
            s += secd_mem2sexp( mem.g_mem[idx]['car'], NESTED + [ idx ] )
        if mem.g_mem[idx]['cdr'] < 0:
            s += " . " + mem.g_symtable[-mem.g_mem[idx]['cdr']] + ")"
        else:
            tail = secd_mem2sexp( mem.g_mem[idx]['cdr'], NESTED + [ idx ] )
            if "" == tail:
                s += ")"
            else:
                s += " " + tail[1:]
        return s


def secd_sexp2mem(txt):
    """
    Store text representation of a funx sexp into cell memory.
    :param txt: funx sexp
    :return: index of cell representation
    """
    def issymchar( c ):
        return c.isalnum() or '&'==c

    def normalize_str(txt):
        """
        Syntactic parse returning a list of tokens.
        :param txt:
        :return: list of tokens
        """
        str_norm = []
        last_c = None
        for c in txt:
            if issymchar(c):
                if issymchar(last_c):
                    str_norm[-1] += c
                else:
                    str_norm.append(c)
            elif not c.isspace():
                str_norm.append(c)
            last_c = c
        return str_norm

    tokens = normalize_str(txt)
    nonlocal = {'token': 0}
    # print tokens

    def getexp( last=1 ):
        # print "exp", tokens[nonlocal['token']]
        if '(' == tokens[nonlocal['token']]:
            nonlocal['token'] += 1
            ret = getexplist( last )
            nonlocal['token'] += 1
        else:
            ret = -mem.secd_sym_get_create(tokens[nonlocal['token']])
            nonlocal['token'] += 1
        return ret

    def getexplist( last=1 ):
        # print "explist", tokens[nonlocal['token']], last
        car = getexp( last )
        idx = mem.secd_mem_setcar(mem.secd_mem_newcell( last ), car)
        if '.' == tokens[nonlocal['token']]:
            nonlocal['token'] += 1
            cdr = getexp( idx+1 )
            idx = mem.secd_mem_setcdr(idx, cdr)
        else:
            if ')' == tokens[nonlocal['token']]:
                if 0 == mem.g_mem[idx]['car'] and 0 == mem.g_mem[idx]['cdr']:
                    idend = mem.secd_mem_newcell()
                    idx = mem.secd_mem_setcdr(idx, idend)
                    idx = idend
                else:
                    idx = mem.secd_mem_setcdr(idx, mem.NIL)
            else:
                idx = mem.secd_mem_setcdr(idx, getexplist( idx+1 ))
        return idx

    return getexp()
