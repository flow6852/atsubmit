.PHONY: build install clean test foruser
build :
	@stack build

install:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}

clean:
	@stack clean

reinstall:
	@stack build

test:
	@stack test
