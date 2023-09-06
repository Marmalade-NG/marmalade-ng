.PHONY: all doc tests clean

all: doc tests

doc:
		make -C doc html

tests:
		make -C tests

clean:
		make -C deployment clean
		make -C doc clean
