
%%is display the debug info
-define(DEBUG, true).
-ifdef(DEBUG).
-define(dbg1(Data), io:format("*dbg ~p:~p: ~p ~n", [?MODULE, ?LINE, Data])).
-define(dbg2(Fmt,Data), io:format("*dbg ~p:~p: "++Fmt++"~n ", [?MODULE, ?LINE | Data])).
-else.
-define(dbg1(Data),"").
-define(dbg2(Fmt,Data),"").
-endif.
%%------------------------



%%---accepter  config------------
-define(LISTEN_PORT,11111).
-define(SERVERS, 16). % how many process to listen Port
%%-------------------------------------------

%%------memcache or redis address and port ------
-define(TARGET_HOST,"127.0.0.1").
-define(TARGET_PORT,11311).
%-define(TARGET_PORT,6379).
%%-----------------------------------------------


%%---mpm config like apache------------------------
-define(POOLS, 16).   % how many pools will you start
-define(POOL_PREFIX,"pooler_").
-define(MIN_POOL_NUMBERS,5).  % min links for per pool
-define(MAX_POOL_NUMBERS,20).  % max links for per pool
%%----------------------------------------------------
