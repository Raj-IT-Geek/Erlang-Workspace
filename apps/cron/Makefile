.PHONY: default all clean compile shell

rebar='rebar3'

default: compile
all: clean compile
build: test release
compile:
	@$(rebar) compile
clean:
	@$(rebar) clean
shell:
	@$(rebar) shell
