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
			io:format("client : ~p ~n",[Any])
	end.


parse_binary(Binary,Status) ->
	io:format("recv: ~p ~n",[binary_to_list(Binary)]),
	%io:format("recv: ~p ~n",[Binary]),
	{auth,Value} = lists:keyfind(auth,1,Status),
	case Value of
		false ->
			%<<_Command:32/integer,EventId:32/integer,Rest/binary>> = Binary,
			%io:format("command = ~p,EventId = ~p,Rest = ~p ~n",[Command,EventId,Rest]),
			{ok,EventId,Client} = protocol:auth(Binary),
			case handle_protocol:auth(Client) of
				ok ->
					NewStatus = lists:keyreplace(auth,1,Status,{auth,true}),
					Response = protocol:response(EventId,?OK),
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
	List = protocol:parse_binary(Binary),
	case protocol:command(List) of
		?POST ->
			{ok,Post} = protocol:post(List),
			handle_post(Post,Status);
		?POSTDATA ->
			{ok,PostData} = protocol:post_data(List),
			handle_post_data(PostData,Status);
		?GET ->
			{ok,Get} = protocol:get(List),
			handle_get(Get,Status);
		Any ->
			io:format("~p ~n",[Any])
	end.
	

handle_post(#post{id = Id,len=L} = Post,Status) ->
	case handle_protocol:post(Post) of
		ok ->
			Response = protocol:post_response(list_to_integer(Id),?OK),
			<<_EventId:32,_Code:32,Indetify/binary>> = Response,
			%% {binary_to_list(Indetify),{Length,Recved_Count}} 
			%% Length is total-size of the file
			%% Recved_Count represents count of receve from socket 
			{ok,Response,[{binary_to_list(Indetify),{L,0}}|Status]};
		{error,Code} ->
			Response = protocol:response(list_to_integer(Id),Code),
			{error,Response,Status}
	end.

handle_post_data(#post_data{id=Id,description=Desc,value=V} = Data,Status) ->
  case handle_protocol:post_data(Data) of
	ok ->
	  Response = protocol:post_data_response(list_to_integer(Id),?OK,Desc),
	  {Desc,{Total,Count}} = lists:keyfind(Desc,1,Status),
	  NewCount = Count + length(V),
	  if 
		Total > NewCount ->
		  NewStatus = lists:keyreplace(Desc,1,Status,{Desc,{Total,NewCount}});
		Total =:= NewCount ->
		  NewStatus = lists:keydelete(Desc,1,Status)
	  end,
	  {ok,Response,NewStatus};
	{error,Code} ->
	  Response = protocol:response(list_to_integer(Id),Code),
	  {error,Response,Status}
  end.


handle_get(#get{id=Id,filename=FileName}=Get,Status) ->
  case handle_protocol:get(Get) of
	ok ->
	  Response = protocol:response(list_to_integer(Id),?OK),
	  {ok,Response,Status};
	{error,Code} ->
	  Response = protocol:response(list_to_integer(Id),Code),
	  {error,Response,Status}
  end.



























