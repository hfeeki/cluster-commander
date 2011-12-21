all: clean getdeps compile link

getdeps:
	@./rebar get-deps

compile:
	@./rebar compile
	@cp deps/*/ebin/*.beam ebin/

link:
	@./rebar escriptize

clean:
	@./rebar clean
