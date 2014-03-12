-module(server).

-include("base.hrl").

-export([start/1]).

-define(Host,"localhost").

start(Port) ->
	{ok,LS} = gen_tcp:listen(Port,
													[binary,
													{packet,4},
													{active,false}]),
	io:format("begining listen [~p] ~n",[Port]),
	spawn(fun() -> accept(LS) end).


accept(LS) ->
	{ok,ClientSocket} = gen_tcp:accept(LS),
	io:format("new client [~p] ~n",[ClientSocket]),
	spawn(fun() -> client(ClientSocket,[{auth,false}]) end),
	accept(LS).

client(ClientSocket,Status) ->
	io:format("process ... ~n"),
	case gen_tcp:recv(ClientSocket,0) of
		{ok,Binary} ->
			NewStatus = parse_binary(Binary,Status),
			client(ClientSocket,NewStatus);
		{error,closed} ->
			gen_tcp:close(ClientSocket);
		Any ->
			io:format("~p ~n",[Any])
	end.


parse_binary(Binary,Status) ->
	io:format("recv: ~p ~n",[binary_to_list(Binary)]),
	{auth,Value} = lists:keyfind(auth,1,Status),
	case Value of
		false ->
			<<Command:32/integer,EventId:32/integer,Rest/binary>> = Binary,
			io:format("command = ~p,EventId = ~p,Rest = ~p ~n",[Command,EventId,Rest]),
			protocol:auth(Rest),
			NewStatus = lists:keyreplace(auth,1,Status,{auth,true}),
			NewStatus;
		true ->
			io:format("handle else ... ~n"),
			Status
	end.
