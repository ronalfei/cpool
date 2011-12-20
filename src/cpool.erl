%% Feel free to use, reuse and abuse the code in this file.

-module(cpool).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(lager),
	application:start(cpool).
	

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
%			{[<<"websocket">>], websocket_handler, []},
			{'_', default_handler, []}
		]}
	],
	cowboy:start_listener(http, 2,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
%% we don't need ssl protocal
%
%	cowboy:start_listener(https, 100,
%		cowboy_ssl_transport, [
%			{port, 8443}, {certfile, "priv/ssl/cert.pem"},
%			{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
%		cowboy_http_protocol, [{dispatch, Dispatch}]
%	),

	cpool_sup:start_link().

stop(_State) ->
	ok.
