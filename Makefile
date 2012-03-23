REBAR=./rebar


all: clean getdeps compile link doc-all

build: compile link

getdeps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile
	@cp  deps/getopt/ebin/getopt.beam        ebin/
	@cp  deps/mochiweb/ebin/mochijson2.beam  ebin/
	@cp  deps/ejson/ebin/ejson.beam          ebin/

link:
	@$(REBAR) escriptize skip_deps=true
	@mkdir -p bin
	@mv commander bin/

doc:
	@$(REBAR) doc skip_deps=true

doc-all:
	@$(REBAR) doc

clean:
	@$(REBAR) clean
	@rm -rf {bin,ebin,doc}

test:
	@$(REBAR) compile eunit skip_deps=true

test-all:
	@$(REBAR) compile eunit
