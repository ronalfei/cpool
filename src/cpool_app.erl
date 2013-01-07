%%% -------------------------------------------------------------------
%%% Author  :   BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2011-7-11
%%% -------------------------------------------------------------------
-module(cpool_app).

-include("cpool.hrl").
-author("BlackAnimal <ronalfei@gmail.com> or <ronalfei@qq.com>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, _} = ranch:start_listener(cpool_server, 4,
			ranch_tcp, [{port, ?LISTEN_PORT}], server_protocol, []),
    cpool_sup:start_link().

stop(_State) ->
    ok.
