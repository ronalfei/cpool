%%% -------------------------------------------------------------------
%%% Author  :	BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2011-1-28
%%% -------------------------------------------------------------------
-module(cpool_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("cpool.hrl").
%% --------------------------------------------------------------------
%% External exports
%-export([start/0, start_link/0, stop/0, get_socket/0, free_socket/1,get_status/0]).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(states, {port, process}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->       
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).
    %gen_server:start( ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


tick() ->
	gen_server:cast(?MODULE, tick).


stop() ->
    gen_server:call(?MODULE, stop).
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Port = ?LISTEN_PORT,

    Process = server_listen(Port),

    {ok, #states{port=Port, process=Process}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(stop, _From, State) ->
	S = #states.port,
	Reply = gen_tcp:close(S),
    {reply, Reply, State};	

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(tick, State) ->
    io:format("~p ticking \n", [?MODULE]),
    {noreply, State};


handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    io:format("terminate, reason: ~p ~n", [Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
server_listen(Port) ->
	case gen_tcp:listen(Port, [binary, {active,true}, {packet,0}]) of
		{ok, ListenSocket} ->
			?dbg2("ListenSocket: ~p ~n", [ListenSocket]),
			mult_accept(?SERVERS, ListenSocket),
			ListenSocket;
		{error,Reason} ->
			Reason
	end.

server_accept(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			spawn(fun() -> server_accept(ListenSocket) end ),
			?dbg1("spawning OK."),
			?MODULE:server_loop(Socket),
			?dbg2("Server accepted:~p ,goto Loop", [Socket]); 
		Other -> ?dbg2("accept return: ~p ~n",[Other])
	end.

server_loop(Socket) ->
	receive
		{tcp, Socket, Data} -> 
			?dbg2("Server recevied data: ~p", [Data]),	
			PoolName = get_rand_pool_name(),
			PoolSocket = cpool_pooler:get_socket(PoolName),
			Respond = cpool_connect:raw(PoolSocket,Data),		
			?dbg2("respond: ~p", [Respond]),	
			gen_tcp:send(Socket, Respond),
			server_loop1(PoolName, Socket, PoolSocket);
		{tcp_closed, Socket} ->
			?dbg2("Socket ~p closed(~p)  ~n ",[Socket,self()]);
		Error -> 
			?dbg2("Server Received unKown ~p ",[Error])
	end.

server_loop1(PoolName, Socket, PoolSocket) -> 
	receive
		{tcp, Socket, Data} -> 
			?dbg2("recevied data: ~p", [Data]),	
			Respond = cpool_connect:raw(PoolSocket,Data),
			?dbg2("respond: ~p", [Respond]),			
			gen_tcp:send(Socket, Respond),
			server_loop1(PoolName, Socket, PoolSocket);
		{tcp_closed, Socket} ->
			cpool_pooler:free_socket(PoolName, PoolSocket),
			?dbg2("Socket ~p closed(~p)  ~n ",[Socket,self()])
	end.
		

mult_accept(0, _ListenSocket) ->
	{ok, true};
mult_accept(Numbers, ListenSocket) ->
	?dbg2("mult_accpte, number: ~p", [Numbers]),
	spawn_link(?MODULE, server_accept, [ListenSocket]),
	?dbg1("server_accept spawned ok"),
	mult_accept(Numbers-1, ListenSocket).

%-------------internal function -------------------
get_rand_pool_name() ->
	{_,_,S} = time(),
	S1 = (S rem ?POOLS )+1,
	?dbg2("S1 = ~p ~n",[S1]),
	S2 = ?POOL_PREFIX ++ integer_to_list(S1),
	S3 = list_to_existing_atom(S2),
	?dbg2("S3 = ~p ~n",[S3]),
	S3.

%% -----------------------------------------------------------------
%%% for test function
%% -----------------------------------------------------------------
%%test() ->
%%	%cpool:start(),
%%	%LS = pfpm_server:start(),
%%	case gen_tcp:connect("127.0.0.1",11111, [binary, {active, false}, {packet,0}],5) of
%%		{ok,Socket} ->
%%			?dbg1("Connect Server Success"), 
%%			gen_tcp:send(Socket,"set ronalfei 0 0 8 \r\nbatisfei\r\n"),
%%			?dbg1("Send Over,wait for receive!~.."),
%%			case gen_tcp:recv(Socket,0) of
%%				{ok, Data} -> ?dbg2("Set : reply from server ~p ~n",[ Data]);
%%				Other -> ?dbg2("Set test recv: ~p ",[Other])
%%			end,
%%			gen_tcp:send(Socket,"get ronalfei\r\n"),
%%			case gen_tcp:recv(Socket,0) of
%%				{ok, Data1} -> ?dbg2("Get 1 reply from server ~p ~n",[ Data1]);
%%				Other1 -> ?dbg2("Get 1 test recv: ~p ",[Other1])
%%			end,
%%			gen_tcp:send(Socket,"get batisfei\r\n"),
%%			case gen_tcp:recv(Socket,0) of
%%				{ok, Data2} -> ?dbg2("Get 2 reply from server ~p ~n",[ Data2]);
%%				Other2 -> ?dbg2("Get 2 test recv: ~p ",[Other2])
%%			end,
%%			?dbg1("Now,close it"),
%%			gen_tcp:close(Socket),
%%			?dbg1("Closed");
%%		{error,Reason} -> ?dbg2("Error Reason:~p ",[Reason])
%%	end.
%%	%cpool:stop(),
%%	%pfpm_server:stop(LS).
%%test2() ->
%%	case gen_tcp:connect("127.0.0.1",11111, [binary, {active, false}, {packet,0}],5) of
%%		{ok,Socket} ->
%%			?dbg1("Connect Server Success"), 
%%			gen_tcp:send(Socket,"set ronalfei 0 0 8 \r\nbatisfei\r\n"),
%%			?dbg1("Send Over,wait for receive!~.."),
%%			case gen_tcp:recv(Socket,0) of
%%				{ok, Data} -> ?dbg2("Set : reply from server ~p ~n",[Data]);
%%				Other -> ?dbg2("Set test recv: ~p ",[Other])
%%			end,
%%			gen_tcp:send(Socket,"get ronalfei\r\n"),
%%			case gen_tcp:recv(Socket,0) of
%%				{ok, Data1} -> ?dbg2("Get 1 reply from server ~p ~n",[ Data1]);
%%				Other1 -> ?dbg2("Get 1 test recv: ~p ",[Other1])
%%			end,
%%			gen_tcp:send(Socket,"get batisfei\r\n"),
%%			case gen_tcp:recv(Socket,0) of
%%				{ok, Data2} -> ?dbg2("Get 2 reply from server ~p ~n",[ Data2]);
%%				Other2 -> ?dbg2("Get 2 test recv: ~p ",[Other2])
%%			end,
%%			?dbg1("Now,close it"),
%%			gen_tcp:close(Socket),
%%			?dbg1("Closed");
%%		{error,Reason} -> ?dbg2("Error Reason:~p ",[Reason])
%%	end.
