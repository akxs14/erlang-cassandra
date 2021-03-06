ERL 	= $(shell which erl)
REBAR = $(shell which rebar)
ERLFLAGS= -pa $(CURDIR)/apps/*/ebin -pa $(CURDIR)/deps/*/ebin

# Fetches all dependencies and compiles the project.
deps:
	$(REBAR) get-deps
	$(REBAR) compile

# Updates dependencies and compiles the project.
update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

# Compiles the project, cleans old test results and runs
# the application's unit tests (EUnit). It doesn't run
# the dependencies' unit tests.
eunit:
	$(REBAR) compile clean-common-test-data
	$(REBAR) skip_deps=true eunit

# Compiles the project and builds the release.
release:
	$(REBAR) compile
	cd rel
	$(REBAR) generate

# Compiles the project and builds the release and starts the
# Erlang REPL with the project running in the background.
#
#	Hint: See if everything started properly with
#		application:which_applications().
#
console:
	$(REBAR) compile
	cd rel
	$(REBAR) generate
	sh rel/device_info/bin/device_info console


# Starts a shell session with the device_info application and
# all its dependencies loaded.
shell:
	$(REBAR) compile
	erl $(ERLFLAGS) -s device_info_app shell -eco_auto_init true

deb-package:
	rebar get-deps
	$(REBAR) compile
	cd rel && $(REBAR) generate

	rm -rf deb/etc/device_info
	cp -r rel/device_info deb/etc

	cp -r conf deb/etc/device_info

	make -C ./deps/debbie

	erl -noshell \
			-pa `pwd`/deps/debbie/ebin \
			-pa `pwd`/deps/debbie/deps/edgar/ebin \
			-pa `pwd`/deps/debbie/deps/swab/ebin \
			-eval 'debbie:fy([{root_path, "deb/"}]), init:stop()'

	mv deb/debian.deb deb/device_info.deb
