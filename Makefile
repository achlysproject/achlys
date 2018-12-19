REBAR            ?= $(shell which rebar3)
BASE_DIR         ?= $(shell pwd)
GRISPAPP         ?= $(shell basename `find src -name "*.app.src"` .app.src)
GRISP_TEST_SRC_DIR	?= $(BASE_DIR)/_build/default/lib/grisp/test/
REBAR_CONFIG	 ?= $(BASE_DIR)/rebar.config
GRISPFILES_DIR		?= $(BASE_DIR)/grisp/grisp_base/files
CACHE_DIR         ?= $(HOME)/.cache/rebar3
COOKIE           ?= MyCookie
VERSION 	       ?= 0.2.0
PDFDIR			?=	$(BASE_DIR)/doc/internal

REBAR_APPEND	?= {extra_src_dirs, [\"$(GRISP_TEST_SRC_DIR)\"]}.

.PHONY: compile shell testshell deploy \
	clean buildclean grispclean cacheclean ⁠fullclean testsrc rel addemu rmvemu
	# cleaning targets :
	# command-line utils


all: compile

##
## Compilation targets
##

compile:
	$(REBAR) compile

##
## Cleaning targets
##

clean: buildclean relclean
	$(REBAR) clean

⁠fullclean: buildclean grispclean cacheclean relclean
	$(REBAR) clean

buildclean:
	rm -rdf $(BASE_DIR)/_build/*/lib/*/ebin/*

grispclean:
	rm -rdf $(BASE_DIR)/_grisp

hexclean:
	rm -rdf $(CACHE_DIR)/hex/hexpm/packages/*

cacheclean:
	rm -rdf $(CACHE_DIR)/lib/*/ebin/*

relclean:
	rm -rdf $(BASE_DIR)/_build/*/rel
#
# Test targets
#

shell: addemu
	$(REBAR) as test shell --sname $(GRISPAPP)$(n) --setcookie $(COOKIE) --apps $(GRISPAPP)

testshell: addemu
	$(REBAR) as test shell --sname $(GRISPAPP)$(n) --setcookie $(COOKIE) --apps $(GRISPAPP) ;

addemu:
	@if [ -f $(REBAR_CONFIG) ]; then \
		if [ $$(grep -c 'extra_src_dirs' rebar.config) -eq 0 ]; then \
			echo $(REBAR_APPEND) >> $(REBAR_CONFIG); \
		fi; \
	else \
		echo "ERROR: no rebar"; \
	fi

## Infinite loop failure with rmvemu target
## rmvemu:
## 	echo sed '/extra_src_dirs/d' $(REBAR_CONFIG) >> $(REBAR_CONFIG)

## Release targets
##

rel:
	$(REBAR) release -d false

deploy:
	$(REBAR) grisp deploy -n $(GRISPAPP) -v $(VERSION)

htmldoc:
	xsltproc --noout --stringparam outdir /tmp/myhtmldoc \
	      --stringparam docgen $(ERL_TOP)/lib/erl_docgen \
              --stringparam topdocdir . \
              --stringparam pdfdir "$(PDFDIR)" \
              --xinclude \
	      --stringparam gendate "October 31 2018" \
              --stringparam appname Achlys \
              --stringparam appver 0.1.0 \
              -path $(ERL_TOP)/lib/erl_docgen/priv/dtd \
              -path $(ERL_TOP)/lib/erl_docgen/priv/dtd_html_entities \
	      $(ERL_TOP)/lib/erl_docgen/priv/xsl/db_html.xsl mybook.xml

include tools.mk
