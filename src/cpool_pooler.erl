%%% -------------------------------------------------------------------
%%% Author  :   BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2011-7-11
%%% -------------------------------------------------------------------
-module(cpool_pooler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("cpool.hrl").

%% --------------------------------------------------------------------
%% External exports
%-export([start/0, start_link/0, stop/0, get_socket/0, free_socket/1, status/0]).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(INTERVAL_TIME, 9000).

-record(states, {sockets, numbers}).


%% ====================================================================
%% External functions
%% ====================================================================
start(PoolName) ->       
    gen_server:start({local, PoolName}, ?MODULE, [], []).

start_link(PoolName) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [], []).

stop(PoolName) ->
    gen_server:call(PoolName, stop).

get_socket(PoolName) ->
	%?dbg2("Get Socket PoolName : ~p ~n",[PoolName]),
	gen_server:call(PoolName, get_socket).

free_socket(PoolName,Socket) ->
	%?dbg2("Free Socket PoolName : ~p ~n",[PoolName]),
	gen_server:cast(PoolName, {free_socket, Socket}).

status(PoolName) -> 
	gen_server:call(PoolName, status).
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
	Numbers = ?MIN_POOL_NUMBERS,
	Sockets = connect([], Numbers),
	?dbg2("Creating pool ~p Ok ", [Numbers]),
	%?dbg2("create pool OK,numbers: ~p ,Sockets : ~p ",[Numbers,Sockets]),
	timer:send_interval(?INTERVAL_TIME, self(), check_connect),

    {ok, #states{sockets=Sockets, numbers=Numbers}}.

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
handle_call(stop,_From, State) ->
	{stop, shutdown,stoped, State};

handle_call(get_socket, _From, State) ->
	Sockets = State#states.sockets,
	Numbers = State#states.numbers,

	if
		Numbers == 0 ->
			case cpool_connect:connect(get_connect_config(), false) of
				{ok, LastSocket} ->
					?dbg2("Dynamic Create Pool Socket Ok : ~p ", [LastSocket]),
					{reply, LastSocket, #states{sockets=[], numbers=0}};
				{error,LastReason} ->
					?dbg2("Dynamic Create Pool Socket Error : ~p ", [LastReason]),
                    {reply, {error, "Dynamic Create Pool Socket Error"}, #states{sockets=[],numbers=0}}
			end;
		Numbers == 1 ->
			[HSocket|_] = Sockets,
			inet:setopts(HSocket, [{active, false}]),
			case cpool_connect:connect(get_connect_config()) of
				{ok, SecondSocket} ->
					?dbg2("Dynamic Create Pool Socket Ok : ~p ", [SecondSocket]),
					{reply, HSocket, #states{sockets=[SecondSocket], numbers=1}};
				{error,SecondReason} ->
					?dbg2("Dynamic Create Pool Socket Error : ~p ", [SecondReason]),
                    {reply, HSocket, #states{sockets=[], numbers=0}}
			end;
		true -> 
			[HSocket | TSocket] = Sockets,
			inet:setopts(HSocket, [{active, false}]),
			{reply, HSocket, #states{sockets=TSocket, numbers=Numbers-1}}
	end;

handle_call(status, _From, State) ->
	Socket_list = State#states.sockets,
	Socket_nums = State#states.numbers,
	{ reply, {Socket_nums, Socket_list} , State};


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

handle_cast({free_socket, Socket}, State) ->
	Sockets = State#states.sockets,
	Numbers = State#states.numbers,
	%?dbg2("Free Socket:~p ", [Socket]),
	case Socket of 
		{error,_} ->
			{noreply, #states{ sockets=Sockets, numbers=Numbers } };
		_ ->
			inet:setopts(Socket, [{active, once}]),
			{noreply, #states{ sockets=[Socket|Sockets], numbers=Numbers+1 }} 
	end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({tcp_closed, Sock}, State) ->
    ?dbg2("Closing connection ~p  due to server termination", [Sock]),
	Sockets  = State#states.sockets,
	NewSocks = lists:delete(Sock, Sockets),
    Numbers  = State#states.numbers,
	self() ! reconnect,
	{noreply, #states{ sockets=NewSocks, numbers=Numbers-1 }};

handle_info({tcp_error, Error, Sock}, State) ->
    ?dbg2("Closing connection ~p due to network error: ~p", [Sock, Error]),
	Sockets  = State#states.sockets,
	NewSocks = lists:delete(Sock, Sockets),
    Numbers  = State#states.numbers,
	self() ! reconnect,
	{noreply, #states{ sockets=NewSocks, numbers=Numbers-1 }};

handle_info(reconnect, State) ->
	?dbg1("reconnecting........"),
    Sockets  = State#states.sockets,
    Numbers  = State#states.numbers,
    NewSocks = reconnect(),
    {noreply, #states{ sockets=[NewSocks|Sockets], numbers=Numbers+1 }};


handle_info(check_connect, State) ->
	?dbg1("starting check connect"),
    Sockets  = State#states.sockets,
    Numbers  = State#states.numbers,
	if 
		Numbers >= ?MAX_POOL_NUMBERS ->
			[HSocket | TSocket] = Sockets,
			%%send a close signal to  memcache Server is necessary
			gen_tcp:close(HSocket),
			?dbg2("close Socket: ~p",[HSocket]),
			?dbg1("done................."),
    		{noreply, #states{ sockets=[TSocket], numbers=Numbers-1 }};
		 true  ->
			?dbg1("done................."),
		 	{noreply, State}
	end;

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
 	%[gen_tcp:close(X) || X <- State#states.sockets],
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


connect(Socket_lists, 0) ->
    Socket_lists;

connect(Socket_lists, Pool_numbers) ->
	Config = get_connect_config(),
    case cpool_connect:connect(Config) of
        {ok,Socket} ->
            connect([Socket|Socket_lists], Pool_numbers-1);
        {error, Reason} ->
            ?dbg2("Connect Error: ~p, Pool_number: ~p ", [Reason, Pool_numbers]),
			receive
            after 2000 ->
            	connect([Socket_lists], Pool_numbers-1)
            end
    end.

reconnect() ->
	Config = get_connect_config(),
	case cpool_connect:connect(Config) of
        {ok,Socket} ->
			?dbg1("reconnecting ok .............."),
			Socket;
        {error, Reason} ->
            ?dbg2("Connect Error: ~p ", [Reason]),
			receive
			after 2000 ->
            	reconnect()
			end
    end.

get_connect_config() ->
    Host = ?TARGET_HOST,
    Port = ?TARGET_PORT,
    [{host,Host},{port,Port},{timeout,5}].


