# SECD Machine project
# Python 2.x implementation

import sys
import argparse

# A SECD VM

import secd_mem   as mem
import secd_parse as parse
import secd_alist as alist
import secd_am    as am

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='funx VM.\n(c) jmc 2021.')
    parser.add_argument('--log', type=argparse.FileType('w'), help='logfile (optional)')
    parser.add_argument('--env',  help='global environment (optional)')
    parser.add_argument('--verbose', default=False, action='store_true')
    parser.add_argument('infile', nargs='?', type=argparse.FileType('r'),
                        default = sys.stdin, help='SECD source file')
    parser.add_argument('--outfile', type=argparse.FileType('w'),
                        default = sys.stdout, help='result of execution (optional)')
    args = parser.parse_args()

    secd_source = args.infile.read()
    # Init
    secd = am.SECDAbstractMachine()
    secd.C = parse.secd_sexp2mem(secd_source)
    if None != args.env :
        secd.E = parse.secd_sexp2mem( args.env )
    secd.E = alist.secd_alist_push(-mem.secd_sym_get_create('*SECDAM*'), -mem.secd_sym_get_create('1.0'), secd.E)
    secd.cycle(verbose=( args.verbose ))
    args.outfile.write(parse.secd_mem2sexp(secd.S))

    if None != args.log:
        args.log.write( '%d cycles.\n' % secd.ncycles )
        args.log.write("Load: %3.2f %%\n" % (100.0*mem.secd_mem_load()/mem.g_MEMSIZE))
        args.log.write("Garbage collector (%d cells)\n" % len(mem.secd_mem_gc( secd )))
        args.log.write("Load: %3.2f %%\n" % (100.0 * mem.secd_mem_load() / mem.g_MEMSIZE))
        args.log.close()