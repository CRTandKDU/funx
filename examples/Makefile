EMACS = "C:\Program Files\Emacs\i686\bin\runemacs.exe"
NLIBS = C:/Users/jmc/Documents/code/funx
FPATH = C:/Users/jmc/Documents/code/funx/examples
NCOMP = C:/Users/jmc/Documents/code/funx/comp

run1: ex1.out
run3: ex3.out

comp1: comp1.out
comp2: comp2.out
comp3: comp3.out


ex1.out  : ex1.fasl
	$(EMACS) -nw -l $(NLIBS)/secd-run.el -batch --eval="(secd-run \"$(FPATH)/$^\" '((a . 12) (b . 5)))"

ex1.fasl : ex1.lsp
	$(EMACS) -nw -l $(NCOMP)/secd-comp.el -batch --eval="(secd-compile \"$(FPATH)/$^\")"

ex3.out  : ex3.fasl
	$(EMACS) -nw -l $(NLIBS)/secd-run.el -batch --eval="(secd-run \"$(FPATH)/$^\" '((a . 12) (b . 5)))"

ex3.fasl : ex3.lsp
	$(EMACS) -nw -l $(NCOMP)/secd-comp.el -batch --eval="(secd-compile \"$(FPATH)/$^\")"

comp1.out  : comp1.fasl
	$(EMACS) -nw -l $(NLIBS)/secd-run.el -batch --eval="(secd-run \"$(FPATH)/$^\" '((a . 12) (b . 5)))"

comp1.fasl : comp1.lsp
	$(EMACS) -nw -l $(NCOMP)/secd-comp.el -batch --eval="(secd-compile \"$(FPATH)/$^\")"

comp2.out  : comp2.fasl
	$(EMACS) -nw -l $(NLIBS)/secd-run.el -batch --eval="(secd-run \"$(FPATH)/$^\" '((a . 12) (b . 5)))"

comp2.fasl : comp2.lsp
	$(EMACS) -nw -l $(NCOMP)/secd-comp.el -batch --eval="(secd-compile \"$(FPATH)/$^\")"

comp3.out  : comp3.fasl
	$(EMACS) -nw -l $(NLIBS)/secd-run.el -batch --eval="(secd-run \"$(FPATH)/$^\" '((a . 12) (b . 5)))"

comp3.fasl : comp3.lsp
	$(EMACS) -nw -l $(NCOMP)/secd-comp.el -batch --eval="(secd-compile \"$(FPATH)/$^\")"
