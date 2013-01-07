-module(server_protocol).
-include("cpool.hrl").
-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(ListenerPid),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Bin} ->
			Data = get_all_socket_buffer(Transport, Socket, Bin, <<>>),
			PoolName = get_rand_pool_name(),
			PoolSocket = cpool_pooler:get_socket(PoolName),
			Respond = case cpool_connect:raw(PoolSocket, Data) of
				{error, closed} -> gen_tcp:close(PoolSocket), <<"Pool connection closed\r\n">> ; %close it,if the PoolSocket is not alived;
				{_Any, Ret} -> 
					?dbg2("what is any: ~p", [_Any]),
					Ret 
			end,

			Transport:send(Socket, Respond),
			loop1(Socket, Transport, PoolName, PoolSocket);
		_ ->
			ok = Transport:close(Socket)
	end.

loop1(Socket, Transport, PoolName, PoolSocket) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Bin} ->
			Data = get_all_socket_buffer(Transport, Socket, Bin, <<>>),
            Respond = case cpool_connect:raw(PoolSocket,Data) of
                {error, closed} -> gen_tcp:close(PoolSocket), <<"Pool connection closed\r\n">> ; %close it,if the PoolSocket is not alived;
                {_Any, Ret} -> 
                    ?dbg2("what is any: ~p", [_Any]),
                    Ret 
            end,
            Transport:send(Socket, Respond),
            loop1(Socket, Transport, PoolName, PoolSocket);
        _ ->
            cpool_pooler:free_socket(PoolName, PoolSocket),
            ok = Transport:close(Socket)
    end.



%-------------internal function -------------------
get_rand_pool_name() ->
	{_, _, S} = time(),
	S1 = (S rem ?POOLS )+1,
	?dbg2("S1 = ~p ~n", [S1]),
	S2 = ?POOL_PREFIX ++ integer_to_list(S1),
	S3 = list_to_existing_atom(S2),
	?dbg2("S3 = ~p ~n", [S3]),
	S3.

get_all_socket_buffer(Transport, Socket, Bin, AllBin) ->
	case binary:part(Bin, {byte_size(Bin), -2}) of 
		<<"\r\n">> -> <<AllBin/binary, Bin/binary>>;
		_Any -> 
			{ok, NewBin} = Transport:recv(Socket, 0, 5000),
			get_all_socket_buffer(Transport, Socket, NewBin, <<AllBin/binary, Bin/binary>>)
	end.
