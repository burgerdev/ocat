
all: build test examples

build:
	jbuilder build

test:
	jbuilder runtest

examples:
	jbuilder build @examples

docs:
	jbuilder build @doc

clean:
	rm -rf .build
	rm -f *.native
	jbuilder clean

.PHONY: build all test examples docs clean
