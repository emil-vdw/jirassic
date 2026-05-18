FILES ?= ./test/*.el

.PHONY: clean package install lint test
clean:
	eask clean all

package:
	eask package

install:
	eask install-deps --dev

lint:
	eask compile --strict
	eask lint checkdoc

test:
	eask test ert $(FILES)
