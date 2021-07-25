# SECD Machine project
# Python 2.x implementation

import numpy as np
import secd_gc as gc

# C-style implementation of lists for a SECD Machine.
# See also: Henderson, Functional Programming p. 291 et sq.; Lispkit
# Alternate implementation using positive or negative indices in slots.
#
global g_mem, g_symtable, g_consdepth
global NIL, TRUE, FALSE, KW_REST, KW_SKP

# Cell memory: row 0 is CAR, row 1 is CDR
g_MEMSIZE  = 30000
g_mem_dt   = np.dtype([('car', np.int16), ('cdr', np.int16)])
g_mem      = np.zeros(g_MEMSIZE, dtype=g_mem_dt)
g_consdepth= 1
g_gc       = gc.secd_gc_init( g_mem )

# Symbol name memory
g_symtable = []


class SECDMemFullError(Exception):
    """Exception raised for errors in cell allocation.

        Attributes:
            expression -- input expression in which the error occurred
            message -- explanation of the error
        """

    def __init__(self, expression, message):
        self.expression = expression
        self.message = message


def secd_sym_find(str):
    if str in g_symtable:
        return g_symtable.index(str)
    else:
        return None


def secd_sym_get_create(str):
    res = secd_sym_find(str)
    if None == res:
        res = len(g_symtable)
        g_symtable.append(str)
    return res


def secd_mem_releasecell( idx ):
    """
    Returns cell to the non allocated pool in memory.
    :param idx: cell index
    :return: index
    """
    # print "MEM release>", idx
    return secd_mem_setcdr( secd_mem_setcar(idx, 0), 0)


def secd_mem_gc( secd ):
    """
    Starts garbage collection on instance of SECD Abstract Machine.
    :param secd: the machine to mark and collect
    :return: GC internal marked array
    """
    m_load = secd_mem_load()
    print "GC> Load: %6d %3.2f %%" % (m_load, 100.0*m_load/g_MEMSIZE)

    ignore = gc.secd_gc_mark(secd.S, g_mem)
    ignore = gc.secd_gc_mark(secd.E, g_mem)
    ignore = gc.secd_gc_mark(secd.C, g_mem)
    ignore = gc.secd_gc_mark(secd.D, g_mem)
    ignore = gc.secd_gc_collect(secd_mem_releasecell)

    m_load = secd_mem_load()
    print "GC> Load: %6d %3.2f %%" % (m_load, 100.0*m_load/g_MEMSIZE)
    return ignore

def secd_mem_decref( idx ):
    print "Decrement", idx
    return 0


def secd_mem_incref ( idx ):
    print "Increment", idx
    return 0


def secd_mem_len(idx, seen=[]):
    ret = 1
    while 0 != g_mem[idx]['cdr']:
        if idx in seen:
            return ret
        ret += 1
        seen += [ idx ]
        idx = g_mem[idx]['cdr']
    return ret

def secd_mem_load():
    load = 0
    for idx in range(g_MEMSIZE):
        if (g_mem[idx]['car'] != 0 or g_mem[idx]['cdr'] != 0 ):
            load += 1
    return load


def secd_mem_newcell( start=1 ):
    """Finds first free cell in global cell memory."""
    idx = start
    while (idx < (g_MEMSIZE )) and \
            (g_mem[idx]['car'] != 0 or g_mem[idx]['cdr'] != 0 ):
        idx += 1
    if idx < (g_MEMSIZE ):
        return idx
    else:
        raise SECDMemFullError('%d' % idx , 'Cell memory full')


def secd_mem_setcar(idx, idx_car):
    g_mem[idx]['car'] = idx_car
    return idx


def secd_mem_setcdr(idx, idx_cdr):
    g_mem[idx]['cdr'] = idx_cdr
    return idx


def secd_mem_cons( idcar, idcdr):
    """
    Cons buildup where car is a symbol or list, and cdr always a list or NIL
    :param idcar: index of symbol in symtable or of list in cell memory
    :param idcdr: always an index in cell memory
    :return:
    """
    global g_consdepth
    idx = secd_mem_setcdr(secd_mem_setcar( secd_mem_newcell(g_consdepth), idcar ), idcdr )
    if NIL == idcdr and NIL == idcar:
        g_consdepth = idx+1
    return idx


def secd_mem_append( idelt, idlist ):
    """
    Appends to end of list.
    :param idelt: symbol or list to insert at end
    :param idlist: list to be extended
    :return: the extended list
    """
    idx = idlist
    while 0 != g_mem[idx]['cdr'] :
        idx = g_mem[idx]['cdr']
    g_mem[idx]['cdr'] = secd_mem_cons( idelt, NIL )
    return idlist

# Some built-in constants
NIL  = secd_sym_get_create('nil') # Has to be equal to 0
NOP  = NIL
TRUE = -secd_sym_get_create('True')
FALSE= -secd_sym_get_create('False')
KW_REST=-secd_sym_get_create('&rest')
KW_SKP=-secd_sym_get_create('&skp')

