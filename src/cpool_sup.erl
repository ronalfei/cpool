%%% -------------------------------------------------------------------
%%% Author  :   BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2011-7-11
%%% -------------------------------------------------------------------
-module(cpool_sup).
-author("BlackAnimal <ronalfei@gmail.com> or <ronalfei@qq.com>").

-behaviour(supervisor).

-include("cpool.hrl").
%% External exports
-export([start_link/0, start/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
	start_link().
	
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	
	Server = {cpool_server,
        {cpool_server, start, []},
        permanent, 5000, worker, dynamic},

	Pools = get_pools(?POOLS, [Server]),
	
?dbg2("Pools : ~p",[Pools]),

    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Pools)}}.

%------------------------------------------------

get_pools(0, Pools) ->
	Pools;
get_pools(N,Pools) -> 
	PoolName = list_to_atom(?POOL_PREFIX ++ integer_to_list(N)),
?dbg2("PoolName = ~p", [PoolName]),
	H = {PoolName,
			{cpool_pooler, start, [PoolName]},
			permanent, 5000, worker, dynamic},
	get_pools(N-1,[H|Pools]).

