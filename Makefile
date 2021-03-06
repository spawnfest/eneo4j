all: setup_db all_checks format show_cover report_cover

all_checks: compile run_type_checks run_tests

precommit: all_checks format build_docs

setup_db:
	docker-compose up -d

compile:
	rebar3 compile

format:
	rebar3 fmt

build_docs:
	rebar3 edoc

run_type_checks:
	rebar3 xref
	rebar3 gradualizer
	rebar3 dialyzer

run_tests:
	rebar3 ct --cover
	rebar3 eunit --cover

show_cover:
	rebar3 cover

report_cover:
	rebar3 codecov analyze
