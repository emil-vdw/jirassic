FILES ?= ./test/*.el

.PHONY: clean package install lint test
clean:
	eask clean all

package:
	eask package

install:
	eask install

lint:
	eask compile --strict
	eask lint checkdoc

test:
	eask compile
	eask test ert $(FILES)
