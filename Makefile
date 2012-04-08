REBAR=./rebar
PLT_FILE=./.dialyzer.plt


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

dialyze: compile
	dialyzer \
		--plt \
			$(PLT_FILE) \
		-c \
			ebin \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wbehaviours \
		-Wunderspecs

build_plt: compile
	dialyzer \
		--build_plt \
		--output_plt \
			$(PLT_FILE) \
		--apps \
			compiler \
			crypto \
			edoc \
			erts \
			et \
			eunit \
			gs \
			hipe \
			inets \
			kernel \
			mnesia \
			observer \
			public_key \
			runtime_tools \
			sasl \
			ssh \
			ssl \
			stdlib \
			syntax_tools \
			tools \
			webtool \
			xmerl
