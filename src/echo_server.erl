-module(echo_server).
-author("shaxunyeman@gmail.com").

-export([start/1]).

-define(Host,"localhost").

start(Port) ->
	{ok,LS} = gen_tcp:listen(Port,[binary,{packet,4},{active,false}]),
	io:format("begining listen [~p] ~n",[Port]),
	spawn(fun() -> accept(LS) end).


accept(LS) ->
	{ok,ClientSocket} = gen_tcp:accept(LS), 
	io:format("new client [~p] ~n",[ClientSocket]),
	spawn(fun() -> echo_client(ClientSocket) end),
	accept(LS).


echo_client(Socket) ->	
	io:format("process ...~n"),
	case gen_tcp:recv(Socket,0) of
		{ok,Binary} ->
			io:format("echo >> ~p~n",[binary_to_list(Binary)]),
			gen_tcp:send(Socket,Binary),
			echo_client(Socket);
		{error,closed} ->
			io:format("closed client [~p]~n",[Socket]),
			gen_tcp:close(Socket)
	end.
