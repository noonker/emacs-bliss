EMACS ?= emacs
# Extra load paths, e.g. LOAD_PATH="-L ~/.emacs.d/straight/build/cond-let"
LOAD_PATH ?=

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -L test $(LOAD_PATH) -l test/bliss-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . $(LOAD_PATH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile bliss.el

clean:
	rm -f *.elc test/*.elc
