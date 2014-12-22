PACKAGE := auto-package-update

EMACS := emacs
CURL := curl --silent

DASH_URL=https://raw.githubusercontent.com/magnars/dash.el/master/dash.el

.PHONY: test

package: *.el
	@ver=`grep -o "Version: .*" $(PACKAGE).el | cut -c 10-`; \
	tar cjvf $(PACKAGE)-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

clean:
	@rm -rf $(PACKAGE)-*/ $(PACKAGE)-*.tar* *.elc

.downloads:
	${CURL} ${DASH_URL} > dash.el
	touch .downloads

test: .downloads
	${EMACS} -Q --batch -L .  -L ./tests \
		-l tests/$(PACKAGE)-test.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"
