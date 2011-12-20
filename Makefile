all: getdeps compile

getdeps:
	@./rebar get-deps

compile:
	@./rebar compile
	@cp deps/*/ebin/*.beam ebin/

clean:
	@./rebar clean
