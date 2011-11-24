#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
#make clean && make \

exec erl -pa ebin \
	-sname cpool \
    -boot start_sasl \
	-s cpool \
	-s reloader\
	-detached
