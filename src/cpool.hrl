-define(dbg1(Data), io:format("*dbg ~p:~p: ~p ~n", [?MODULE, ?LINE, Data])).
-define(dbg2(Fmt,Data), io:format("*dbg ~p:~p: "++Fmt++"~n ", [?MODULE, ?LINE | Data])).


%-define(dbg1(Data),"").
%-define(dbg2(Fmt,Data),"").

-define(LISTEN_PORT,11111).


-define(TARGET_HOST,"127.0.0.1").
%-define(TARGET_HOST,"10.100.6.152").
-define(TARGET_PORT,11211).

-define(POOLS,5).   % how many pools will you start
-define(POOL_PREFIX,"pooler_").

-define(SERVERS,5). % how many process to listen Port

-define(MIN_POOL_NUMBERS,2).  % min links for per pool
-define(MAX_POOL_NUMBERS,20).  % max links for per pool
