BIN_NAME = atsubmit
LOCAL_BIN_PATH = $(HOME)/.local/bin
LOCAL_CACHE_PATH = $(HOME)/.cache/$(BIN_NAME)
LOCAL_CONF_PATH = $(HOME)/.config/$(BIN_NAME)
LOCAL_MAN_PATH = $(HOME)/.local/share/man
LOCAL_LIB_PATH = $(HOME)/.local/lib/$(BIN_NAME)

.PHONY: build install clean test forvim
build :
	@stack build
	@sudo install $(shell stack exec -- which $(BIN_NAME)-server) $(LOCAL_BIN_PATH)/$(BIN_NAME)-server
	@sudo install $(shell stack exec -- which $(BIN_NAME)-client) $(LOCAL_BIN_PATH)/$(BIN_NAME)-client
	@cp sample/lang_conf.json $(LOCAL_CONF_PATH)
test:
	@stack test

install:
	@stack build
	@if [ ! -d $(LOCAL_BIN_PATH) ]; then mkdir $(LOCAL_BIN_PATH); fi
	@if [ ! -d $(LOCAL_CACHE_PATH) ]; then mkdir $(LOCAL_CACHE_PATH) ; fi
	@if [ ! -d $(LOCAL_CACHE_PATH)/src ]; then mkdir $(LOCAL_CACHE_PATH)/src ; fi
	@if [ ! -d $(LOCAL_CONF_PATH) ]; then mkdir $(LOCAL_CONF_PATH) ; fi
	@if [ ! -d $(LOCAL_LIB_PATH) ]; then mkdir $(LOCAL_LIB_PATH) ; fi
	@if [ ! -d $(LOCAL_MAN_PATH) ]; then mkdir $(LOCAL_MAN_PATH) ; fi
	@sudo install $(shell stack exec -- which $(BIN_NAME)-exe) $(LOCAL_BIN_PATH)/$(BIN_NAME)
	@if [ $(shell docker image ls atjudge_hs | wc -l) -eq 1 ]; then docker build docker/hs_container -t atsubmit_hs --memory 1024m ; fi
	@if [ $(shell docker image ls atjudge_rs | wc -l) -eq 1 ]; then docker build docker/rs_container -t atsubmit_rs --memory 1024m ; fi
	@cp doc/help.man $(LOCAL_MAN_PATH)/$(BIN_NAME).man
	@cp docker/docker_judge.sh $(LOCAL_LIB_PATH)
	@cp sample/lang_conf.json $(LOCAL_CONF_PATH)
	@echo "installed"

update:
	@cp docker/docker_judge.sh $(LOCAL_LIB_PATH)

uninstall:
	@rm -rf $(LOCAL_LIB_PATH) $(LOCAL_CACHE_PATH) $(LOCAL_CONF_PATH) $(LOCAL_MAN_PATH)/$(BIN_NAME).man $(LOCAL_BIN_PATH)/$(BIN_NAME)
	@docker rmi atsubmit_hs
	@docker rmi atsubmit_rs

test-clean:
	@rm $(LOCAL_BIN_PATH)/$(BIN_NAME)-test

clean:
	@stack clean

forvim:
	@cp -r asvim $(HOME)/.vim/plugged
