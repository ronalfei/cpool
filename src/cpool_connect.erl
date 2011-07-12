-module(cpool_connect).
-compile(export_all).
-include("cpool.hrl").


%Compatible Memcached Error strings
%-------------
%
%Each command sent by a client may be answered with an error string
%from the server. These error strings come in three types:
%
%- "ERROR\r\n" 
%
%  means the client sent a nonexistent command name.
%
%- "CLIENT_ERROR <error>\r\n"
%
%  means some sort of client error in the input line, i.e. the input
%  doesn't conform to the protocol in some way. <error> is a
%  human-readable error string.
%
%- "SERVER_ERROR <error>\r\n"
%
%  means some sort of server error prevents the server from carrying
%  out the command. <error> is a human-readable error string. In cases
%  of severe server errors, which make it impossible to continue
%  serving the client (this shouldn't normally happen), the server will
%  close the connection after sending the error line. This is the only
%  case in which the server closes a connection to a client.




config() ->
	Host = ?TARGET_HOST,
	Port = ?TARGET_PORT,
	[{host,Host},{port,Port},{timeout,5}].

connect(Config) ->		
	[{host,Host},{port,Port},{timeout,Timeout}] = Config,
	%?dbg2("Connecting... Host: ~p,Port : ~p ",[Host,Port]),
	gen_tcp:connect(Host,Port,[binary,{packet,0},{active,false}],Timeout). 


raw(Socket,RawData) ->
	case Socket of 
		{error,EReason} ->
			?dbg2("Get socket from pool failed: ~p",[EReason]),
			<<"CLIENT_ERROR <Get socket from pool failed>\r\n">>;
		_ ->
			gen_tcp:send(Socket,RawData),
		    {_,Ret} = gen_tcp:recv(Socket,0),
    		Ret
	end.

%-----------for memcache client------------------
get(Socket,Key) ->
	gen_tcp:send(Socket,"get "++Key++"\r\n"),
	case gen_tcp:recv(Socket,0) of
		{ok,Packet} -> 
			parse_get(Packet); 
		{error,Reason} ->?dbg2("Get error : ~p ",[Reason])
		end.
set(Socket,Key,Value)->
	set(Socket,Key,Value,0,0).

set(Socket,Key,Value,Exptime)->
	set(Socket,Key,Value,0,Exptime).

set(Socket,Key,Value,Flags,Exptime) ->
	Length		= string:len(Value),
	Exptime1	= integer_to_list(Exptime),
	Length1		= integer_to_list(Length),
	Command		= "set "++Key++" "++integer_to_list(Flags)++" "++ Exptime1 ++" "++ Length1 ++" \r\n"++Value++"\r\n",
	%?dbg2(" send chunk: ~p ~n",[Command]),
	gen_tcp:send(Socket,Command),
	case gen_tcp:recv(Socket,0) of
		{ok,<<"STORED\r\n">>} -> success;
		{ok,<<"NOT_STORED\r\n">>} -> failed,?dbg1("Not store");
		{error,Reason} ->?dbg2("Set Value Error : ~p ",[Reason])
	end.

mget(Socket,Lkey) ->
	L = [X++" "||X <- Lkey],
	S = lists:concat(L),
	gen_tcp:send(Socket,"get "++S++"\r\n"),
	case gen_tcp:recv(Socket,0) of
		{ok,Packet} -> parse_mget(Packet);
		{error,Reason} ->?dbg2("mGet Value error : ~p ",[Reason])
    end.


close(Socket) ->
	gen_tcp:close(Socket).

parse_get(Packet) ->
	case string:tokens(binary_to_list(Packet),"\r\n")  of
		[_,Result,_] -> Result;
		[_] -> null
	end.
parse_mget(Packet) ->
	L = string:tokens(binary_to_list(Packet),"\r\n"),
	parse_mlist(L).

parse_mlist(["END"])->[];
parse_mlist( [H1, H2 |Tail] ) ->
	K = lists:nth(2, string:tokens(H1," ") ),
	V = H2,
	[ {K, V} | parse_mlist(Tail)].

%%---------test-----------------------
test() ->
%	?MODULE:guess(),
	Config = ?MODULE:config(),
	case ?MODULE:connect(Config) of 
		{ok,Socket} ->
			?dbg2("Socket:  ~p",[Socket]),
			?MODULE:set(Socket,"a","AA"),
			?MODULE:set(Socket,"b","BB"),
			?MODULE:set(Socket,"c","CC",3),
			?MODULE:set(Socket,"d","DD"),
			?MODULE:set(Socket,"e","EE"),
			?MODULE:set(Socket,"f","FF"),

			L = ["a","b","c","d","e","f"],
			Ret1 	=	?MODULE:mget(Socket,L),
			Ret2	=	?MODULE:get(Socket,"a"),
			 
			?dbg2("~n Ret1: ~p ~n Ret2: ~p ~n",[Ret1,Ret2]),
			?MODULE:close(Socket);
		{error,Reason} -> ?dbg2("Error Reason :~p",[Reason])
	end.




