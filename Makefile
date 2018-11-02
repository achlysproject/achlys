REBAR            ?= $(shell which rebar3)
BASE_DIR         ?= $(shell pwd)
GRISPAPP         ?= $(shell basename `find src -name "*.app.src"` .app.src)
GRISPFILES_DIR		?= $(BASE_DIR)/grisp/grisp_base/files
CACHE_DIR         ?= $(HOME)/.cache/rebar3
COOKIE           ?= MyCookie
VERSION 	       ?= 0.1.0
DEPLOY_DEST		?=	/media/laymer/GRISP
PDFDIR			?=	$(BASE_DIR)/doc/internal

.PHONY: compile shell testshell deploy \
	clean buildclean grispclean cacheclean ⁠fullclean testsrc rel
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

shell:
	$(REBAR) as test shell --sname $(GRISPAPP)$(n) --setcookie $(COOKIE) --apps $(GRISPAPP)

##
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
