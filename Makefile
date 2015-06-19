
REBAR = ~/local/bin/rebar
ERL = erl -pa ebin -pa deps/exml/ebin -pa deps/expellee/ebin -pa deps/ranch/ebin -pa deps/eper/ebin
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

dialyze: compile
	$(DIALYZE_SH)

dialyze-clean:
	$(RM) dialysis/*.plt
	$(RM) dialysis/*.plt.log
