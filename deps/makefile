DIRS = rebar cowlib ranch cowboy goldrush lager jsx meck protobuffs riak_pb riakc \
		 quickrand uuid


.PHONY: all clean

all:
	for d in $(DIRS); do (cd $$d; make) || exit 1 ; done

clean:
	for d in $(DIRS); do (cd $$d; make clean) || exit 1; done
