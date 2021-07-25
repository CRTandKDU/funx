# SECD Machine project
# Python 2.x implementation

import secd_mem   as mem
import secd_parse as parse
import secd_alist as alist
import secd_am    as am

if __name__ == '__main__':
    # Init
    secd = am.SECDAbstractMachine(logfile='secdsecd.log')
    secd.C = parse.secd_sexp2mem(
        # '(DUM '
        # 'LDF ((x) '
        #     'LD x '
        #     'LDE (LDC 1 LD x ADD LDF ((n) LD n LD terms AP RTN) AP UPD) '
        #     'CONS RTN) '
        # 'LDF ((terms) LDC 1 LD terms AP CDR AP0 CDR AP0 CDR AP0 CAR RTN) '
        # 'RAP STOP)'

        # '(DUM '
        # 'LDF ((n) LD n LDC 1 EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD ping AP LD n MUL JOIN) RTN) '
        # 'LDF ((n) LD n LDC 1 EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD pong AP LD n MUL JOIN) RTN) '
        # 'LDF ((n) LD n LDC 1 EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD pang AP LD n MUL JOIN) RTN) '
        # 'LDF ((ping pang pong) LDC 5 LD ping AP RTN) '
        # 'RAP STOP)'

        # '(DUM LDF ((elist) LD nil LD elist EQ SEL (LD nil JOIN) (LD elist CAR CAR LD elist CDR LD secdcompvars AP CONS JOIN) RTN) LDF ((elist n c) LD nil LD elist EQ SEL (LD c JOIN) (LD c LD n LD elist CAR CDR CAR LD secdcomp AP LD n LD elist CDR LD secdcomplist AP JOIN) RTN) LDF ((e n c) LD e ATOM SEL (LDC LD LD e LD c CONS CONS JOIN) (LDC car LD e CAR EQ SEL (LDC CAR LD c CONS LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC cdr LD e CAR EQ SEL (LDC CDR LD c CONS LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC atom LD e CAR EQ SEL (LDC ATOM LD c CONS LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC quote LD e CAR EQ SEL (LDC LDC LD e CDR CAR LD c CONS CONS JOIN) (LDC cons LD e CAR EQ SEL (LDC CONS LD c CONS LD n LD e CDR CDR CAR LD secdcomp AP LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC eq LD e CAR EQ SEL (LDC EQ LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC leq LD e CAR EQ SEL (LDC LEQ LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC add LD e CAR EQ SEL (LDC ADD LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC sub LD e CAR EQ SEL (LDC SUB LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC mul LD e CAR EQ SEL (LDC MUL LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC div LD e CAR EQ SEL (LDC DIV LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC rem LD e CAR EQ SEL (LDC REM LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC if LD e CAR EQ SEL (LDC (JOIN) LD n LD e CDR CDR CDR CAR LD secdcomp AP LDC (JOIN) LD n LD e CDR CDR CAR LD secdcomp AP LDF ((cont-t cont-f) LDC SEL LD cont-t LD cont-f LD c CONS CONS CONS LD n LD e CDR CAR LD secdcomp AP RTN) AP JOIN) (LDC lambda LD e CAR EQ SEL (LDC LDF LD e CDR CAR LDC (RTN) LD n LD e CDR CDR CAR LD secdcomp AP CONS LD c CONS CONS JOIN) (LDC let LD e CAR EQ SEL (LDC DUM LDC LDF LD e CDR CAR LD secdcompvars AP LDC (RTN) LD n LD e CDR CDR CAR LD secdcomp AP CONS LDC RAP LD c CONS CONS CONS LD n LD e CDR CAR LD secdcomplist AP CONS JOIN) (LDC AP LD c CONS LD n LD e CAR LD secdcomp AP LD n LD e CDR LD secd-comp--args AP JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) RTN) LDF ((secdcomp secdcomplist secdcompvars) LDC (STOP) LD nil LDC  LD secdcomp AP RTN) RAP STOP)'
        # '(LDC (add (quote 3) (mul (quote 2) (quote 3))) STOP)'
        # '(DUM LDF ((n) LDC 1 LD n EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD fac AP LD n MUL JOIN) RTN) LDF ((fac) LDC 3 LD fac AP RTN) RAP STOP)'
        # '(DUM LDF ((elist n c) LD elist LD nil EQ SEL (LD c JOIN) (LD c LD n LD elist CAR LD secdcomp AP LD n LD elist CDR LD secdcompargs AP JOIN) RTN) LDF ((elist) LD elist LD nil EQ SEL (LD nil JOIN) (LD elist CAR CAR LD elist CDR LD secdcompvars AP CONS JOIN) RTN) LDF ((elist n c) LD elist LD nil EQ SEL (LD c JOIN) (LD c LD n LD elist CAR CDR CAR LD secdcomp AP LD n LD elist CDR LD secdcomplist AP JOIN) RTN) LDF ((e n c) LD e ATOM SEL (LDC LD LD e LD c CONS CONS JOIN) (LDC car LD e CAR EQ SEL (LDC CAR LD c CONS LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC cdr LD e CAR EQ SEL (LDC CDR LD c CONS LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC atom LD e CAR EQ SEL (LDC ATOM LD c CONS LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC quote LD e CAR EQ SEL (LDC LDC LD e CDR CAR LD c CONS CONS JOIN) (LDC cons LD e CAR EQ SEL (LDC CONS LD c CONS LD n LD e CDR CDR CAR LD secdcomp AP LD n LD e CDR CAR LD secdcomp AP JOIN) (LDC eq LD e CAR EQ SEL (LDC EQ LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC leq LD e CAR EQ SEL (LDC LEQ LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC add LD e CAR EQ SEL (LDC ADD LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC sub LD e CAR EQ SEL (LDC SUB LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC mul LD e CAR EQ SEL (LDC MUL LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC div LD e CAR EQ SEL (LDC DIV LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC rem LD e CAR EQ SEL (LDC REM LD c CONS LD n LD e CDR CAR LD secdcomp AP LD n LD e CDR CDR CAR LD secdcomp AP JOIN) (LDC if LD e CAR EQ SEL (LDC (JOIN) LD n LD e CDR CDR CDR CAR LD secdcomp AP LDC (JOIN) LD n LD e CDR CDR CAR LD secdcomp AP LDF ((contt contf) LDC SEL LD contt LD contf LD c CONS CONS CONS LD n LD e CDR CAR LD secdcomp AP RTN) AP JOIN) (LDC lambda LD e CAR EQ SEL (LDC LDF LD e CDR CAR LDC (RTN) LD n LD e CDR CDR CAR LD secdcomp AP CONS LD c CONS CONS JOIN) (LDC let LD e CAR EQ SEL (LDC DUM LDC LDF LD e CDR CAR LD secdcompvars AP LDC (RTN) LD n LD e CDR CDR CAR LD secdcomp AP CONS LDC RAP LD c CONS CONS CONS LD n LD e CDR CAR LD secdcomplist AP CONS JOIN) (LDC AP LD c CONS LD n LD e CAR LD secdcomp AP LD n LD e CDR LD secdcompargs AP JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) JOIN) RTN) LDF ((secdcomp secdcomplist secdcompvars secdcompargs) LD nil LD nil LDC (let ((inc (lambda (x) (add x (quote 1))))) (inc (quote 3))) LD secdcomp AP RTN) RAP STOP)'
        # '(DUM LDF ((x) LDC 1 LD x ADD RTN) LDF ((inc) LDC 3 LD inc AP RTN) RAP STOP)'
        # '(DUM LDF ((n) LDC 1 LD n EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD fac AP LD n MUL JOIN) RTN) LDF ((fac) LDC 11 LD fac AP RTN) RAP)'
        # '(DUM LDC 2 LDC 1 LDF ((X Y) LD Y LD X ADD RTN) RAP)'

        # '(DUM LDF ((arg) LD arg LD nil EQ SEL (LDC False JOIN) (LDC 1 LD arg CAR LDC True EQ SEL (LDC True JOIN) (LD arg CDR LD or AP JOIN) JOIN) RTN) LDF ((or) LDC (True True) LD or AP RTN) RAP)'
        # '(LDC 1 LD nil CONS STOP)'
            # '(DUM LDF ((x y &rest z) LD y LD x ADD LD z CONS RTN) LDF ((foo) SKP LDC FOO LDC 3 LDC 2 LD foo AP RTN) RAP)'
        # '(LDC S)'
        '(DUM LDF ((x y) LD x LD y LDE (SKP LD y LD x ADD LD y ADD LD y LD x ADD LD fib AP UPD) CONS CONS RTN) LDF ((fib) SKP LDC 1 LDC 1 LD fib AP CDR CDR AP0 CDR CDR AP0 CAR RTN) RAP STOP)'

     )

    secd.E = alist.secd_alist_push( -mem.secd_sym_get_create( 'SOURCE'),
                                    -mem.secd_sym_get_create( 'SOURCE' ),
                                    secd.E)
    # Run
    print secd.state()
    secd.cycle( verbose=True )
    print secd.state()
    print '%d cycles.' % secd.ncycles
    print "Load: %3.2f %%" % (100.0*mem.secd_mem_load()/mem.g_MEMSIZE)
    print "Garbage collector (%d cells)" % len(mem.secd_mem_gc( secd ))
    print "Load: %3.2f %%\n" % (100.0 * mem.secd_mem_load() / mem.g_MEMSIZE)
    print mem.g_symtable

    # assl = mem.NIL
    # assl = alist.secd_alist_push( -mem.secd_sym_get_create( 'KEY'), -mem.secd_sym_get_create( 'VALUE' ), assl)
    # print parse.secd_mem2sexp( assl )

    # assl = parse.secd_sexp2mem( '((A . 1) (B (4 5)))' )
    # assl = alist.secd_alist_push( -mem.secd_sym_get_create( 'KEY'), -mem.secd_sym_get_create( 'VALUE' ), assl)
    # print parse.secd_mem2sexp( assl )
