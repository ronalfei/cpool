%%% -------------------------------------------------------------------
%%% Author  :   BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2011-7-11
%%% -------------------------------------------------------------------



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
%-----------------------------------------


connect(Config) ->
	?MODULE:connect(Config, once). 

connect(Config, Opt) -> 
	[{host,Host},{port,Port},{timeout,Timeout}] = Config,
	gen_tcp:connect(Host,Port,[binary,{packet,0},{active, Opt}, {send_timeout, 5000}],Timeout).

raw(Socket,RawData) ->
	case Socket of 
		{error,EReason} ->
			?dbg2("Get socket from pool failed: ~p",[EReason]),
			{error, <<"CLIENT_ERROR\r\n">>};
		_ ->
            gen_tcp:send(Socket, RawData),
            {ok, Bin} = gen_tcp:recv(Socket, 0),
			%?dbg2("first bin is :~s", [Bin]),
			Respon = memcached_respon(Bin, Socket, <<>>),
			{ok, Respon}
	end.


memcached_respon(Bin, Socket, AllBin) ->
	case binary:part(Bin, {byte_size(Bin), -2}) of
		%<<"END\r\n">> -> <<AllBin/binary, Bin/binary>>;
		%<<"ROR\r\n">> -> <<AllBin/binary, Bin/binary>>; %means ERROR
		%<<"UND\r\n">> -> <<AllBin/binary, Bin/binary>>; %means ERROR
		%<<"RED\r\n">> -> <<AllBin/binary, Bin/binary>>; %means ERROR
		%<<"IST\r\n">> -> <<AllBin/binary, Bin/binary>>; %means ERROR
		%<<"TED\r\n">> -> <<AllBin/binary, Bin/binary>>; %means ERROR
		<<"\r\n">> -> <<AllBin/binary, Bin/binary>>; %means ERROR
		_Any -> 
			{ok, NewBin} = gen_tcp:recv(Socket, 0),
			%?dbg2("New bin is :~s", [Bin]),
			memcached_respon(NewBin, Socket, <<AllBin/binary, Bin/binary>>)
	end.
		
%-----------for memcache client------------------
%get(Socket,Key) ->
%	gen_tcp:send(Socket,"get "++Key++"\r\n"),
%	case gen_tcp:recv(Socket,0) of
%		{ok,Packet} -> 
%			parse_get(Packet); 
%		{error,Reason} ->?dbg2("Get error : ~p ",[Reason])
%		end.
%set(Socket,Key,Value)->
%	set(Socket,Key,Value,0,0).
%
%set(Socket,Key,Value,Exptime)->
%	set(Socket,Key,Value,0,Exptime).
%
%set(Socket,Key,Value,Flags,Exptime) ->
%	Length		= string:len(Value),
%	Exptime1	= integer_to_list(Exptime),
%	Length1		= integer_to_list(Length),
%	Command		= "set "++Key++" "++integer_to_list(Flags)++" "++ Exptime1 ++" "++ Length1 ++" \r\n"++Value++"\r\n",
%	%?dbg2(" send chunk: ~p ~n",[Command]),
%	gen_tcp:send(Socket,Command),
%	case gen_tcp:recv(Socket,0) of
%		{ok,<<"STORED\r\n">>} -> success;
%		{ok,<<"NOT_STORED\r\n">>} -> failed,?dbg1("Not store");
%		{error,Reason} ->?dbg2("Set Value Error : ~p ",[Reason])
%	end.
%
%mget(Socket,Lkey) ->
%	L = [X++" "||X <- Lkey],
%	S = lists:concat(L),
%	gen_tcp:send(Socket,"get "++S++"\r\n"),
%	case gen_tcp:recv(Socket,0) of
%		{ok,Packet} -> parse_mget(Packet);
%		{error,Reason} ->?dbg2("mGet Value error : ~p ",[Reason])
%    end.
%
%
%close(Socket) ->
%	gen_tcp:close(Socket).
%
%parse_get(Packet) ->
%	case string:tokens(binary_to_list(Packet),"\r\n")  of
%		[_,Result,_] -> Result;
%		[_] -> null
%	end.
%parse_mget(Packet) ->
%	L = string:tokens(binary_to_list(Packet),"\r\n"),
%	parse_mlist(L).
%
%parse_mlist(["END"])->[];
%parse_mlist( [H1, H2 |Tail] ) ->
%	K = lists:nth(2, string:tokens(H1," ") ),
%	V = H2,
%	[ {K, V} | parse_mlist(Tail)].
%

