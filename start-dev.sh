#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
#make clean && make \

exec erl -pa ebin -pa deps/*/ebin \
	+K true \
	-sname cpool_dev \
   	-boot start_sasl \
	-s cpool \
	-s reloader
