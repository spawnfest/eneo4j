all: compile format run_type_checks run_tests show_cover

compile:
	rebar3 compile

format:
	rebar3 fmt

run_type_checks:
	rebar3 gradualizer

run_tests:
	rebar3 ct --cover
	rebar3 eunit --cover

show_cover:
	rebar3 cover
