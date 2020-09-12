all: compile run_type_checks run_tests show_cover

compile:
	rebar3 compile

run_type_checks:
	rebar3 gradualizer

run_tests:
	rebar3 ct --cover
	rebar3 eunit --cover

show_cover:
	rebar3 cover
