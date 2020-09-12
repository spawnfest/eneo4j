all: setup_db all_checks format show_cover report_cover

all_checks: compile run_type_checks run_tests

setup_db:
	docker-compose up -d

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

report_cover:
	rebar3 codecov analyze
