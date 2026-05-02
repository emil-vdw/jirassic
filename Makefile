FILES ?= ./test/*.el

.PHONY: install test
install:
	eask install

test: install
	eask test ert $(FILES)
