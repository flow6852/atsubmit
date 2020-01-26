LOCAL_BIN_PATH = $(HOME)/.local/bin
LOCAL_CACHE_PATH = $(HOME)/.cache/atsubmit
LOCAL_CONF_PATH = $(HOME)/.config/atsubmit
LOCAL_MAN_PATH = $(HOME)/.local/share/man
LOCAL_LIB_PATH = $(HOME)/.local/lib/atsubmit
BIN_NAME = atsubmit

.PHONY: build install clean test foruser
build :
	@stack build
test:
	@stack test

install:
	@stack build
	@if [ ! -d $(LOCAL_BIN_PATH) ]; then mkdir $(LOCAL_BIN_PATH); fi
	@if [ ! -d $(LOCAL_CACHE_PATH) ]; then mkdir $(LOCAL_CACHE_PATH) ; fi
	@if [ ! -d $(LOCAL_CONF_PATH) ]; then mkdir $(LOCAL_CONF_PATH) ; fi
	@if [ ! -d $(LOCAL_LIB_PATH) ]; then mkdir $(LOCAL_LIB_PATH) ; fi
	@if [ ! -d $(LOCAL_MAN_PATH) ]; then mkdir $(LOCAL_MAN_PATH) ; fi
	@sudo install $(shell stack exec which $(BIN_NAME)-exe) $(LOCAL_BIN_PATH)/$(BIN_NAME)
	@docker build docker/hs_container -t atjudge_hs --memory 1024m
	@cp doc/help.man $(LOCAL_MAN_PATH)/atsubmit.man
	@cp docker/docker_judge.sh $(LOCAL_LIB_PATH)
	@echo "installed"

update:
	@cp docker/docker_judge.sh $(LOCAL_LIB_PATH)

uninstall:
	@rm -rf $(LOCAL_LIB_PATH) $(LOCAL_CACHE_PATH) $(LOCAL_CONF_PATH) $(LOCAL_MAN_PATH)/atsubmit.man $(LOCAL_BIN_PATH)/atsubmit
	@docker rmi atjudge_hs

clean:
	@stack clean

