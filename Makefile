compile:
	@./rebar compile
	@cp deps/*/ebin/*.beam ebin/

clean:
	@./rebar clean
