FILES ?= ./test/*.el

.PHONY: test
test:
	eask test ert $(FILES)
