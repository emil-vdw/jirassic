FILES ?= ./test/*.el

.PHONY: install lint test
install:
	eask install

lint:
	eask compile --strict
	eask lint checkdoc

test:
	eask test ert $(FILES)
