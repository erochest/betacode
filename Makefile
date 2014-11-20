
FLAGS=--enable-tests

all: init test docs package

init:
	cabal sandbox init
	make deps

test: build
	cabal test --test-option=--color

specs: build
	./dist/build/betacode-specs/betacode-specs

run:
	cabal run

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

release:
	cabal check
	cabal upload `cabal sdist | cut -f 2 -d:`

hlint:
	hlint *.hs src specs

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure ${FLAGS}

deps: clean
	cabal install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	cabal build

restart: distclean init build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint
