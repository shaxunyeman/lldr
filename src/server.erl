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
	spawn(fun() -> client(ClientSocket,[{socket,ClientSocket},{auth,false}]) end),
	accept(LS).

client(ClientSocket,Status) ->
	io:format("process ... ~n"),
	case gen_tcp:recv(ClientSocket,0) of
		{ok,Binary} ->
			%NewStatus = parse_binary(Binary,Status),
			case parse_binary(Binary,Status) of
				{ok,Response,NewStatus} ->
					gen_tcp:send(ClientSocket,Response);
				{error,Response,NewStatus} ->
					gen_tcp:send(ClientSocket,Response)
			end,
			client(ClientSocket,NewStatus);
		{error,closed} ->
			gen_tcp:close(ClientSocket),
			io:format("close ~n");
		Any ->
			io:format("~p ~n",[Any])
	end.


parse_binary(Binary,Status) ->
	%io:format("recv: ~p ~n",[binary_to_list(Binary)]),
	{auth,Value} = lists:keyfind(auth,1,Status),
	case Value of
		false ->
			<<_Command:32/integer,EventId:32/integer,Rest/binary>> = Binary,
			%io:format("command = ~p,EventId = ~p,Rest = ~p ~n",[Command,EventId,Rest]),
			Client = protocol:auth(Rest),
			case parse_protocol:auth(Client) of
				ok ->
					NewStatus = lists:keyreplace(auth,1,Status,{auth,true}),
					Response = protocol:response(EventId,0),
					io:format("response >> ~p ~n",[Response]),
					{ok,Response,NewStatus};
				{error,Any} ->
					io:format("error [~p] ~n",[Any]),
					{error,unkown,Status}
			end;
		true ->
			io:format("handle else ... ~n"),
			{ok,unkown,Status}
	end.
