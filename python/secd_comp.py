# SECD Machine project
# Python 2.x implementation

import sys
import argparse

# A funx compiler (meta-circular)

import secd_mem   as mem
import secd_parse as parse
import secd_alist as alist
import secd_am    as am

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='funx Metacircular Strict Compiler.\n(c) jmc 2021.')
    parser.add_argument('--log', type=argparse.FileType('w'), help='logfile (optional)')
    parser.add_argument('--verbose', default=False, action='store_true')
    parser.add_argument('--alternate', default='compiler.secd', help='alternate compiler secd-file')
    parser.add_argument('infile', type=argparse.FileType('r'), help='funx source file')
    parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'),
                        default = sys.stdout, help='secd compiled file')
    args = parser.parse_args()

    with open(args.alternate, 'r') as f:
        fmcc_template = f.read()

    funx_source = args.infile.read()
    fmcc = fmcc_template % funx_source

    # Init
    secd = am.SECDAbstractMachine(logfile='secdcomp.log')
    secd.C = parse.secd_sexp2mem( fmcc )
    secd.E = alist.secd_alist_push(-mem.secd_sym_get_create('*FMCC*'), -mem.secd_sym_get_create('1.0'), secd.E)
    secd.cycle( verbose=args.verbose )
    args.outfile.write( parse.secd_mem2sexp(mem.g_mem[secd.S]['car']) )

    if None != args.log:
        args.log.write( '%d cycles.\n' % secd.ncycles )
        args.log.write("Load: %3.2f %%\n" % (100.0*mem.secd_mem_load()/mem.g_MEMSIZE))
        args.log.write("Garbage collector (%d cells)\n" % len(mem.secd_mem_gc( secd )))
        args.log.write("Load: %3.2f %%\n" % (100.0 * mem.secd_mem_load() / mem.g_MEMSIZE))
        args.log.close()
    args.infile.close()
    args.outfile.close()
