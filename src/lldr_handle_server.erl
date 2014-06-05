-module(lldr_handle_server).
-author("shaxunyeman@gmail.com").

-export([acceptor/1]).

-include("include/typedef.hrl").

acceptor(ClientSocket) ->
	Status = [{auth,false},{socket,ClientSocket}],
	acceptor(ClientSocket,Status).

acceptor(ClientSocket,Status) ->
	%%error_logger:info_msg("~p:~p <~p> ~n",[?MODULE,?LINE,self()]),
	case gen_tcp:recv(ClientSocket,0) of
		{ok,Binary} ->
			%%error_logger:info_msg("~p:~p <~p> ~p ~n",[?MODULE,?LINE,self(),Binary]),
			case parse_binary(Binary,Status) of
				{ok,Response,NewStatus} ->
					gen_tcp:send(ClientSocket,Response);
				{error,Response,NewStatus} ->
					gen_tcp:send(ClientSocket,Response)
			end,
			acceptor(ClientSocket,NewStatus);
		{error,closed} ->
			%%io:format("A client [~p] has closed",[ClientSocket]),
			%%{ok,{Address,Port}} = inet:peername(ClientSocket),
			%%error_logger:info_msg("~p:~p <~p> A client [~p:~p] has closed ~n",[?MODULE,?LINE,self(),Address,Port]),
			error_logger:info_msg("~p:~p <~p> A client has closed ~n",[?MODULE,?LINE,self()]),
			gen_tcp:close(ClientSocket);
		Any ->
			%% io:format("A client [~p] has occurs error,the reason is ~p ~n",[ClientSocket,Any]),
			{ok,{Address,Port}} = inet:peername(ClientSocket),
			error_logger:error_msg("~p:~p <~p> A client [~p:~p] has occurs error,the reason is ~p ~n",[?MODULE,?LINE,self(),Address,Port,Any]),
			gen_tcp:close(ClientSocket)
	end.


parse_binary(Binary,Status) ->
  lldr_handle_pack:parse_binary(Binary,Status).	
