#!/bin/sh
/opt/app/otp_17.1/bin/erl -boot start_sasl -sname cpool@localhost -pa ebin -pa deps/*/ebin -s cpool -s reloader +K true -setcookie cpool\
    -eval "io:format(\"* cpool already started~n~n\")."
