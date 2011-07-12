%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the pfpm application.

-module(cpool_app).

-author("BlackAnimal <ronalfei@gmail.com> or <ronalfei@qq.com>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %application:start(crypto),
    case cpool_sup:start() of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) ->
    ok.
