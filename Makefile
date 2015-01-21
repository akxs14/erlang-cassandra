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
	$(REBAR) get-deps
	$(REBAR) compile
	cd rel
	$(REBAR) generate

console:
	$(REBAR) get-deps
	$(REBAR) compile
	cd rel
	$(REBAR) generate
	chmod a+x rel/device_info/bin/device_info
	./rel/device_info/bin/device_info console
