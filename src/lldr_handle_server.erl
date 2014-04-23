-module(lldr_handle_server).
-author("shaxunyeman@gmail.com").

-export([acceptor/1]).

-include("base.hrl").

acceptor(ClientSocket) ->
	Status = [{auth,false},{socket,ClientSocket}],
	acceptor(ClientSocket,Status).

acceptor(ClientSocket,Status) ->
	case gen_tcp:recv(ClientSocket,0) of
		{ok,Binary} ->
			case parse_binary(Binary,Status) of
				{ok,Response,NewStatus} ->
					gen_tcp:send(ClientSocket,Response);
				{error,Response,NewStatus} ->
					gen_tcp:send(ClientSocket,Response)
			end,
			acceptor(ClientSocket,NewStatus);
		{error,closed} ->
			io:format("A client [~p] has closed",[ClientSocket]),
			gen_tcp:close(ClientSocket);
		Any ->
			io:format("A client [~p] has occurs error,the reason is ~p ~n",[ClientSocket,Any]),
			gen_tcp:close(ClientSocket)
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
					%%Response = protocol:response(EventId,?OK),
					Response = response:ok(EventId),
					io:format("response >> ~p ~n",[Response]),
					{ok,Response,[{client,Client}|NewStatus]};
				{error,Any} ->
					io:format("error [~p] ~n",[Any]),
					%%Response = protocol:response(EventId,?SOCKET_ERROR),
					Response = response:error(EventId,?SOCKET_ERROR),
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
		{ok,{Indetify,{file,IoDevice}}} ->
			%%Response = protocol:post_response(list_to_integer(Id),?OK),
			Response = response:okindentify(list_to_integer(Id),?OK,Indetify),
			%% <<_EventId:32,_Code:32,Indetify/binary>> = Response,

			%% {binary_to_list(Indetify),{Length,Recved_Count}} 
			%% Length is total-size of the file
			%% Recved_Count represents count of receve from socket 
			NewStatus = [{binary_to_list(Indetify),[{L,0},{file,IoDevice}]}|Status],
			{ok,Response,NewStatus};
		{error,Code} ->
			%%Response = protocol:response(list_to_integer(Id),Code),
			Response = response:error(list_to_integer(Id),Code),
			{error,Response,Status}
	end.

handle_post_data(#post_data{id=Id,description=Desc,value=V} = Data,Status) ->
  {Desc,[{Total,Count},{file,IoDevice}]} = lists:keyfind(Desc,1,Status),
  case handle_protocol:post_data(IoDevice,Data) of
	ok ->
	  %%Response = protocol:post_data_response(list_to_integer(Id),?OK,list_to_binary(Desc)),
	  Response = response:okindentify(list_to_integer(Id),?OK,list_to_binary(Desc)),
	  NewCount = Count + length(V),
	  if 
		Total > NewCount ->
		  NewStatus = lists:keyreplace(Desc,1,Status,{Desc,[{Total,NewCount},{file,IoDevice}]});
		Total =:= NewCount ->
		  NewStatus = lists:keydelete(Desc,1,Status)
	  end,
	  {ok,Response,NewStatus};
	{error,Code} ->
	  %%Response = protocol:response(list_to_integer(Id),Code),
	  Response = response:error(list_to_integer(Id),Code),
	  {error,Response,Status}
  end.


handle_get(#get{id=Id,filename=_FileName}=Get,Status) ->
  {client,Client} = lists:keyfind(client,1,Status),
  case handle_protocol:get(Client,Get) of
	{ok,{Indentify,{size,Length}}} ->
	  %%Response = protocol:response(list_to_integer(Id),Length),
	  Response = response:okindentify(list_to_integer(Id),Length,Indentify),
	  {ok,Response,Status};
	  %%push_data_to_client(Status,Response);
	{error,Code} ->
	  %%Response = protocol:response(list_to_integer(Id),Code),
	  Response = response:error(list_to_integer(Id),Code),
	  {error,Response,Status}
  end.


%push_data_to_client(Status,Response) ->  
%  <<EventId:32,Length:32,Indentify/binary>> = Response,
%  case lists:keyfind(socket,1,Status) of
%	{socket,ClientFd} ->
%	  Id = 1000,
%	  Indentify = list_to_binary("push_data_indentify"),
%	  Data = list_to_binary("liuguozhu"),
%	  PushData = <<Id:32,Indentify/binary,0:32,8:32,Data/binary>>,
%	  gen_tcp:send(ClientFd,PushData);
%	false ->
%	  io:format("push_data_to_client : not found socket ~n")
%  end.


























