-module(cpool).
-author("<ronalfei@gmail.com> or <ronalfei@qq.com>").
-compile(export_all).
-include("cpool.hrl").

start() ->
	application:start(crypto),
	application:start(ranch),
	application:start(cpool).
status() ->
	[ {pooler, X, cpool_pooler:status(list_to_existing_atom(?POOL_PREFIX ++ integer_to_list(X)))} || X <- lists:seq(1, ?POOLS) ].
	
test() ->
	?dbg1("test reloader 1---").
