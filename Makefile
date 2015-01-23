ERL 	= $(shell which erl)
REBAR 	= $(shell which rebar)

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

eunit:
	compile clean-common-test-data
	$(REBAR) eunit

release:
	$(REBAR) compile
	cd rel
	$(REBAR) generate

console:
	$(REBAR) compile
	cd rel
	$(REBAR) generate
	sh rel/device_info/bin/device_info console

eunit:
	$(REBAR) skip_deps=true eunit
