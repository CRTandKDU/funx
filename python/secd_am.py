# SECD Machine project
# Python 2.x implementation
import sys
import secd_mem   as mem
import secd_parse as parse
import secd_alist as alist

# Abstract machine
class SECDAMError(Exception):
    """Exception raised for errors in abstract machine execution.

        Attributes:
            expression -- input expression in which the error occurred
            message -- explanation of the error
        """

    def __init__(self, expression, message):
        self.expression = expression
        self.message = message

class SECDAbstractMachine :
    def __init__(self, logfile=None):
        self.mnemonics = {
            # Load Group
            'LDC': self.transition_LDC,
            'LD' : self.transition_LD,
            'LDF': self.transition_LDF,
            'LDE': self.transition_LDE,
            # Arithmetics Group
            'ADD': self.transition_ADD,
            'MUL': self.transition_MUL,
            'SUB': self.transition_SUB,
            'DIV': self.transition_DIV,
            'POW': self.transition_POW,
            'REM': self.transition_REM,
            'EQ' : self.transition_EQ,
            'LEQ': self.transition_LEQ,
            # sexp group
            'ATOM': self.transition_ATOM,
            'CAR' : self.transition_CAR,
            'CDR' : self.transition_CDR,
            'CONS': self.transition_CONS,
            # control group
            'SEL' : self.transition_SEL,
            'JOIN': self.transition_JOIN,
            # Function group
            'AP' : self.transition_AP,
            'RTN': self.transition_RTN,
            'SKP': self.transition_SKP,
            # Recursive block group
            'DUM': self.transition_DUM,
            'RAP': self.transition_RAP,
            # Lazy group
            'UPD': self.transition_UPD,
            'AP0': self.transition_AP0
        }
        self.S = mem.NIL
        self.E = mem.NIL
        self.C = mem.NIL
        self.D = mem.NIL
        # Basic instrumentation
        self.ncycles = 0
        self.stack_depth = 0
        if None != logfile:
            self.logmem = open(logfile, 'w')
        else:
            self.logmem = None

    def env_locate(self, var ):
        # print ("LOCATE %d> " % var) + mem.g_symtable[-var], parse.secd_mem2sexp(self.E)
        return alist.secd_alist_assq( var, self.E )

    def transition_LDC(self):
        self.C = mem.g_mem[self.C]['cdr']
        self.S = mem.secd_mem_cons( mem.g_mem[self.C]['car'], self.S )


    def transition_LD(self):
        self.C = mem.g_mem[self.C]['cdr']
        val = self.env_locate( mem.g_mem[self.C]['car'] )
        self.S = mem.secd_mem_cons( val, self.S )


    def transition_ADD(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        sym = '%d' % (a + b)
        self.S = mem.secd_mem_cons( -mem.secd_sym_get_create(sym), self.S )


    def transition_SUB(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        sym = '%d' % (b - a)
        self.S = mem.secd_mem_cons( -mem.secd_sym_get_create(sym), self.S )


    def transition_MUL(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        sym = '%d' % (a * b)
        self.S = mem.secd_mem_cons( -mem.secd_sym_get_create(sym), self.S )


    def transition_DIV(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        sym = '%d' % (b // a)  # Floor division on integers
        self.S = mem.secd_mem_cons( -mem.secd_sym_get_create(sym), self.S )


    def transition_REM(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        sym = '%d' % (b % a)
        self.S = mem.secd_mem_cons( -mem.secd_sym_get_create(sym), self.S )


    def transition_POW(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        sym = '%d' % ( b ** a )
        self.S = mem.secd_mem_cons( -mem.secd_sym_get_create(sym), self.S )


    def transition_EQ(self):
        ida = mem.g_mem[self.S]['car']
        if ida<0 :
            try:
                a = int( mem.g_symtable[ -ida ] )
            except ValueError as err:
                a = -ida
        else:
            a = ida

        self.S = mem.g_mem[self.S]['cdr']
        idb = mem.g_mem[self.S]['car']
        if idb<0:
            try:
                b = int( mem.g_symtable[ -idb ] )
            except ValueError as err:
                b = -idb
        else:
            b = idb

        self.S = mem.g_mem[self.S]['cdr']
        boolval = mem.TRUE if  (b == a) else mem.FALSE
        self.S = mem.secd_mem_cons( boolval, self.S )


    def transition_LEQ(self):
        a = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        b = int( mem.g_symtable[ -mem.g_mem[self.S]['car'] ] )
        self.S = mem.g_mem[self.S]['cdr']
        boolval = mem.TRUE if  (b < a) else mem.FALSE
        self.S = mem.secd_mem_cons( boolval, self.S )


    def transition_ATOM(self):
        boolval = mem.TRUE if (mem.g_mem[self.S]['car'] <= 0) else mem.FALSE
        self.S = mem.g_mem[self.S]['cdr']
        self.S = mem.secd_mem_cons( boolval, self.S )


    def transition_CAR(self):
        idx = mem.g_mem[ mem.g_mem[self.S]['car'] ]['car']
        self.S = mem.g_mem[self.S]['cdr']
        self.S = mem.secd_mem_cons( idx, self.S )


    def transition_CDR(self):
        idx = mem.g_mem[mem.g_mem[self.S]['car']]['cdr']
        self.S = mem.g_mem[self.S]['cdr']
        self.S = mem.secd_mem_cons(idx, self.S)


    def transition_CONS(self):
        idcdr = mem.g_mem[self.S]['car']
        self.S = mem.g_mem[self.S]['cdr']
        idcar = mem.g_mem[self.S]['car']
        self.S = mem.g_mem[self.S]['cdr']
        idcons = mem.secd_mem_cons( idcar, idcdr )
        self.S = mem.secd_mem_cons( idcons, self.S )


    def transition_SEL( self ):
        idtest = mem.g_mem[self.S]['car']
        self.S = mem.g_mem[self.S]['cdr']
        idnext = mem.g_mem[ mem.g_mem[self.C]['cdr'] ]['car'] \
            if (mem.TRUE == idtest) \
            else mem.g_mem[ mem.g_mem[ mem.g_mem[self.C]['cdr'] ]['cdr'] ]['car']
        idcont = mem.g_mem[ mem.g_mem[ mem.g_mem[self.C]['cdr'] ]['cdr'] ]['cdr']
        self.C = mem.secd_mem_cons( mem.NOP, idnext ) # C is incremented outside after transition
        self.D = mem.secd_mem_cons( idcont, self.D )


    def transition_JOIN(self):
        self.C = mem.secd_mem_cons( mem.NOP, mem.g_mem[self.D]['car'] ) # C is incremented outside after transition
        self.D = mem.g_mem[self.D]['cdr']

    def transition_SKP(self):
        self.S = mem.secd_mem_cons(mem.KW_SKP, self.S)

    def transition_LDF(self):
        self.C = mem.g_mem[self.C]['cdr']
        idfunx = mem.g_mem[self.C]['car']
        self.S = mem.secd_mem_cons( mem.secd_mem_cons( idfunx, self.E ), self.S )


    def transition_AP(self):

        def slurp (idx):
            if mem.KW_SKP == mem.g_mem[idx]['car']:
                return mem.NIL
            else:
                return mem.secd_mem_cons(mem.g_mem[idx]['car'], slurp(mem.g_mem[idx]['cdr']))

        idenv   = self.E
        idargsv = mem.g_mem[self.S]['cdr']
        idargs  = mem.g_mem[ mem.g_mem[ mem.g_mem[self.S]['car'] ]['car'] ]['car']
        idnext  = mem.g_mem[ mem.g_mem[ mem.g_mem[self.S]['car'] ]['car'] ]['cdr']
        self.E  = mem.g_mem[ mem.g_mem[self.S]['car'] ]['cdr']
        while mem.NIL != idargs :
            if mem.KW_REST == mem.g_mem[ idargs ]['car']:
                idargs  = mem.g_mem[ idargs ]['cdr']
                idslurp = slurp(idargsv)
                self.E = alist.secd_alist_push(mem.g_mem[idargs]['car'], idslurp, self.E)
                idargs = mem.g_mem[idargs]['cdr']
            else:
                self.E = alist.secd_alist_push( mem.g_mem[ idargs ]['car'], mem.g_mem[ idargsv]['car'], self.E )
                idargs  = mem.g_mem[ idargs ]['cdr']
                idargsv = mem.g_mem[ idargsv]['cdr']
        self.D = mem.secd_mem_cons( idargsv,
                                    mem.secd_mem_cons( idenv,
                                                       mem.secd_mem_cons( mem.g_mem[self.C]['cdr'], self.D )))
        # self.S = mem.NIL
        # This caused a problem when first sexp pushed to stack is nil, as nil == (nil) == 0 in g_mem
        # We start with a sentinel on the stack, which will be removed by RTN (which keeps only the top)
        self.stack_depth += 1
        # self.S = mem.secd_mem_setcdr(
        #     mem.secd_mem_setcar(
        #         mem.secd_mem_newcell(),-mem.secd_sym_get_create('*STACK-%d*' % self.stack_depth)), mem.NIL)
        self.S = mem.secd_mem_setcdr(
            mem.secd_mem_setcar(
                mem.secd_mem_newcell(),-mem.secd_sym_get_create('*FILLER*')), mem.NIL)
        self.C = mem.secd_mem_cons( mem.NOP, idnext )


    def transition_RTN(self):
        # Ignore the residual KW_SKP!
        idrtn  = mem.g_mem[self.S]['car']
        self.S = mem.secd_mem_cons( idrtn, mem.g_mem[self.D]['car'] )
        self.D = mem.g_mem[self.D]['cdr']
        self.E = mem.g_mem[self.D]['car']
        self.D = mem.g_mem[self.D]['cdr']
        self.C = mem.secd_mem_cons( mem.NOP, mem.g_mem[self.D]['car'] )
        self.D = mem.g_mem[self.D]['cdr']


    def transition_DUM(self):
        self.E = mem.secd_mem_cons( mem.NOP, self.E )


    def transition_RAP(self):
        idenv  = mem.g_mem[self.E]['cdr']
        idargsv= mem.g_mem[self.S]['cdr']
        idargs = mem.g_mem[mem.g_mem[mem.g_mem[self.S]['car']]['car']]['car']
        idnext = mem.g_mem[mem.g_mem[mem.g_mem[self.S]['car']]['car']]['cdr']
        # self.E = mem.g_mem[mem.g_mem[self.S]['car']]['cdr']
        idvals = idenv
        while mem.NIL != idargs:
            idvals  = alist.secd_alist_push(mem.g_mem[idargs]['car'], mem.g_mem[idargsv]['car'], idvals)
            idargs  = mem.g_mem[idargs]['cdr']
            idargsv = mem.g_mem[idargsv]['cdr']
        # print "RAP idvals> " + parse.secd_mem2sexp(idvals)
        # print "RAP self.E> " + parse.secd_mem2sexp(self.E)
        mem.secd_mem_setcar( self.E, mem.g_mem[idvals]['car'] )
        mem.secd_mem_setcdr( self.E, mem.g_mem[idvals]['cdr'] )
        # print "RAP idvals> " + parse.secd_mem2sexp(idvals)
        # print "RAP self.E> " + parse.secd_mem2sexp(self.E)
        self.D = mem.secd_mem_cons(idargsv,
                                   mem.secd_mem_cons(idenv,
                                                     mem.secd_mem_cons(mem.g_mem[self.C]['cdr'], self.D)))
        self.S = mem.NIL
        self.C = mem.secd_mem_cons(mem.NOP, idnext)

    def transition_LDE(self):
        self.C = mem.g_mem[self.C]['cdr']
        idfunx = mem.g_mem[self.C]['car']
        self.S = mem.secd_mem_cons(
            mem.secd_mem_cons( mem.FALSE, mem.secd_mem_cons(idfunx, self.E) ), self.S )

    def transition_AP0(self):
        idpromise = mem.g_mem[self.S]['car']
        idtag     = mem.g_mem[idpromise]['car']
        if mem.TRUE == idtag:
            self.S = mem.secd_mem_cons( mem.g_mem[idpromise]['cdr'], self.S )
        else:
            if mem.FALSE == idtag:
                self.D = mem.secd_mem_cons( self.S,
                                            mem.secd_mem_cons( self.E,
                                                               mem.secd_mem_cons(self.C, self.D)))
                self.C = mem.secd_mem_cons( mem.NIL, mem.g_mem[mem.g_mem[idpromise]['cdr']]['car'] )
                self.E = mem.g_mem[mem.g_mem[idpromise]['cdr']]['cdr']
                self.S = mem.NIL
            else:
                pass

    def transition_UPD(self):
        idp = mem.g_mem[mem.g_mem[self.D]['car']]['car']
        ids = mem.g_mem[mem.g_mem[self.D]['car']]['cdr']
        ide = mem.g_mem[ mem.g_mem[self.D]['cdr'] ]['car']
        idc = mem.g_mem[ mem.g_mem[ mem.g_mem[self.D]['cdr'] ]['cdr'] ]['car']
        idd = mem.g_mem[ mem.g_mem[ mem.g_mem[self.D]['cdr'] ]['cdr'] ]['cdr']
        val = mem.g_mem[self.S]['car']
        self.S = mem.secd_mem_cons( val, ids )
        self.E = ide
        self.C = idc
        self.D = idd
        mem.secd_mem_setcar( idp, mem.TRUE )
        mem.secd_mem_setcdr( idp, val )


    def cycle(self, verbose=False, startncycle=0):
        self.stack_depth = 0
        self.ncycles = startncycle
        try:
            if verbose:
                print "[%d]" % self.ncycles
                print self.state()
            pc = mem.g_mem[self.C]['car']
            while pc < 0 and 0 != (pc + mem.secd_sym_get_create('STOP')) :
                # if 0 == (self.ncycles % 1000):
                #     print "[%6d]" % self.ncycles, mem.secd_mem_load()

                if None != self.logmem:
                    m_load = mem.secd_mem_load()
                    self.logmem.write( '%6d, %2.4f, %5d, %5d\n' % ( self.ncycles, m_load*100./mem.g_MEMSIZE, m_load , mem.secd_mem_len(self.S)))

                # idtop = mem.secd_mem_newcell()
                # if idtop > ((mem.g_MEMSIZE * 50) // 100):
                #     mem.secd_mem_gc(self)

                self.ncycles += 1
                self.mnemonics[mem.g_symtable[-pc]]()
                self.C = mem.g_mem[self.C]['cdr']
                pc = mem.g_mem[self.C]['car']
                if verbose:
                    print "[%d]" % self.ncycles
                    print self.state()
        except mem.SECDMemFullError as err:
            print "SECDMemFullError", err.message
            print self.state()

    def state( self ):
        s =  "S: " + parse.secd_mem2sexp(self.S)[:80] + '\n'
        s += "E: " + parse.secd_mem2sexp(self.E)[:80] + '\n'
        s += "C: " + parse.secd_mem2sexp(self.C)[:80] + '\n'
        s += "D: " + parse.secd_mem2sexp(self.D)[:80] + '\n'
        return s