CASK ?= cask
EMACS ?= emacs

all: unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

.PHONY:	all test unit ecukes install
