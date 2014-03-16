-module(server).

-include("base.hrl").

-export([start/1]).

-define(Host,"localhost").

start(Port) ->
	{ok,LS} = gen_tcp:listen(Port,
													[binary,
													{packet,0},
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
			io:format("client : ~p ~n",[Any])
	end.


parse_binary(Binary,Status) ->
	%io:format("recv: ~p ~n",[binary_to_list(Binary)]),
	io:format("recv: ~p ~n",[Binary]),
	{auth,Value} = lists:keyfind(auth,1,Status),
	case Value of
		false ->
			%<<_Command:32/integer,EventId:32/integer,Rest/binary>> = Binary,
			%io:format("command = ~p,EventId = ~p,Rest = ~p ~n",[Command,EventId,Rest]),
			{ok,EventId,Client} = protocol:auth(Binary),
			case parse_protocol:auth(Client) of
				ok ->
					NewStatus = lists:keyreplace(auth,1,Status,{auth,true}),
					Response = protocol:response(EventId,0),
					io:format("response >> ~p ~n",[Response]),
					{ok,Response,NewStatus};
				{error,Any} ->
					io:format("error [~p] ~n",[Any]),
					Response = protocol:response(EventId,?SOCKET_ERROR),
					{error,Response,Status}
			end;
		true ->
			%%io:format("handle else ... ~n"),
			process_command(Binary,Status)
	end.
	
%%
%% return {ok,Response,NewStatus} or
%%				{error,Response,NewStatus}
%%
process_command(Binary,Status) -> 	
	<<Command:32/integer,Rest/binary>> = Binary,
	case Command of 
		POST ->
			handle_post(Rest,Status);
		GET ->
			handle_get(Rest,Status);
		_ ->
			io:format("process command [~p] ~n",[Command])
	end.


handle_post(Binary,Status) ->
	io:format("handle_get ~n").
	%<<EventId:32/integer,Rest/binary>> = Binary,
	


handle_get(Binary,Status) ->
	io:format("handle_get ~n").


































