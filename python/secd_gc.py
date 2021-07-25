# SECD Machine project
# Python 2.x implementation

import numpy as np
global MARKED

def secd_gc_init( arr ):
    global MARKED
    MARKED = np.zeros_like( arr, dtype=np.uint8 )
    return MARKED

def secd_gc_mark( n, arr ):
    # print "GC>", n
    global MARKED
    if 0==n:
        return MARKED
    if 0 == MARKED[n]:
        MARKED[n] = 1
        car = arr[n]['car']
        if car > 0:
            secd_gc_mark( car, arr )
        cdr = arr[n]['cdr']
        if cdr > 0:
            secd_gc_mark(cdr, arr)
    return MARKED

def secd_gc_collect( callback ):
    global MARKED
    for idx in range(1,len(MARKED)):
        if 0 == MARKED[idx]:
            callback( idx )
        else:
            MARKED[idx] = 0
    return MARKED