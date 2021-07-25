# SECD Machine project
# Python 2.x implementation

# Association lists as stacks of (key . value) pairs

import secd_mem as mem
import secd_parse as parse

def secd_alist_push( key, value, alist ):
    """
    Push a pair into association list
    :param key: a symbol or a list
    :param value: a symbol or a list
    :param alist: the association list
    :return: the extended association list
    """
    idpair = mem.secd_mem_cons( key, value )
    return   mem.secd_mem_cons( idpair, alist )

def secd_alist_assq ( key, alist ):
    """
    Search key in association list and returns value or nil if not found.
    :param key: key for search
    :param alist: association list to be searched
    :return: found value or nil
    """
    idx = alist
    while idx > 0:
        pair = mem.g_mem[idx]['car']

        if key == mem.g_mem[pair]['car']:
            return mem.g_mem[pair]['cdr']
        else:
            idx = mem.g_mem[idx]['cdr']
    return mem.NIL

def secd_alist_setq ( key, val, alist ):
    """
    Change value associated to key in association list.
    :param key: key to search
    :param val: new value for key, if found
    :param alist: association list to be searched
    :return: modified pointed pair or nil if key not found
    """
    idx = alist
    while idx > 0:
        pair = mem.g_mem[idx]['car']
        if key == mem.g_mem[pair]['car']:
            mem.secd_mem_decref( mem.g_mem[pair]['cdr'] )
            mem.g_mem[pair]['cdr'] = val
            return pair
        else:
            idx = mem.g_mem[idx]['cdr']
    return mem.NIL

def secd_alist_delq ( key, alist ):
    """
    Delete first matching key pointed pair in association list
    :param key: key to search for
    :param alist: association list to be searched
    :return: modified association list
    """
    idx, idprev = alist, None
    while idx > 0:
        pair = mem.g_mem[idx]['car']
        if key == mem.g_mem[pair]['car']:
            mem.secd_mem_decref(pair)
            mem.secd_mem_decref(idx)
            if None == idprev :
                return mem.g_mem[idx]['cdr']
            else:
                mem.g_mem[idprev]['cdr'] = mem.g_mem[idx]['cdr']
                return alist
        else:
            idprev = idx
            idx = mem.g_mem[idx]['cdr']
    return alist

def secd_alist_rassq( val, alist ):
    """
    Reverse search for value in association list.
    :param val: value to search for
    :param alist: association list to be searched
    :return: first key found or nil
    """
    idx = alist
    while idx > 0:
        pair = mem.g_mem[idx]['car']
        if val == mem.g_mem[pair]['cdr']:
            return mem.g_mem[pair]['car']
        else:
            idx = mem.g_mem[idx]['cdr']
    return mem.NIL