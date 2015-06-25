
REBAR = ~/local/bin/rebar
ERL = erl \
	-pa deps/eper/ebin -pa deps/goldrush/ebin -pa deps/lager/ebin \
	-pa ebin -pa deps/exml/ebin -pa deps/expellee/ebin \
	-pa deps/ranch/ebin
DIALYZE_SH = dialysis/dialyze.sh
RM = rm -f

all: compile


get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

run: compile
	$(ERL)

test: compile
	$(ERL) -s owl_test prepare -s owl_test test

test-jid: compile
	$(ERL) -s owl_test prepare -s owl_jid_test test

test-session: compile
	$(ERL) -s owl_test prepare -s owl_session_test test

dialyze: compile
	$(DIALYZE_SH)

dialyze-clean:
	$(RM) dialysis/rel.plt
	$(RM) dialysis/rel.plt.log

dialyze-clean-core: dialyze-clean
	$(RM) dialysis/core.plt
	$(RM) dialysis/core.plt.log
