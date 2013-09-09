SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage

.PHONY: test lint doc

all: build

build: cabal.sandbox.config
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

add-sources: cabal.sandbox.config

cabal.sandbox.config:
	cabal sandbox init
