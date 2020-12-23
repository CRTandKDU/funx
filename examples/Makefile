EMACS = "C:\Program Files\Emacs\i686\bin\runemacs.exe"
NLIBS = C:/Users/jmc/Documents/code/funx
FPATH = C:/Users/jmc/Documents/code/funx/examples
NCOMP = C:/Users/jmc/Documents/code/funx/comp

run: ex1.out

ex1.out  : ex1.fasl
	$(EMACS) -nw -l $(NLIBS)/secd-run.el -batch --eval="(secd-run \"$(FPATH)/$^\" '((a . 12) (b . 5)))"

ex1.fasl : ex1.lsp
	$(EMACS) -nw -l $(NCOMP)/secd-comp.el -batch --eval="(secd-compile \"$(FPATH)/$^\")"
