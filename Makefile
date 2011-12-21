all: clean getdeps compile link

getdeps:
	@./rebar get-deps

compile:
	@./rebar compile
	@cp deps/*/ebin/*.beam ebin/

link:
	@./rebar escriptize
	@mkdir -p bin
	@mv commander bin/

clean:
	@./rebar clean
