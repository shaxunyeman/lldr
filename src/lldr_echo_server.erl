-module(lldr_echo_server).
-author("shaxunyeman@gmail.com").

-export([start/0]).
-export([acceptor/1]).
-export([acceptor_once/1]).

start() ->
  lldr_socket_server:start(?MODULE,8001,{?MODULE,acceptor}).

acceptor_once(Socket) ->  
  io:format("lldr_echo_server:acceptor_once ~n"),
  inet:setopts(Socket,[{active,once}]),
  %case gen_tcp:recv(Socket,0) of
  receive
	{tcp,Socket,Packet} ->
	  io:format("echo >> ~p ~n",[Packet]),
	  gen_tcp:send(Socket,Packet),
	  acceptor(Socket);
	{tcp_closed,Socket} ->
	  io:format("A client [~p ] has closed ~n",[Socket]),
	  ok
  end.


acceptor(Socket) ->
  error_logger:info_msg("lldr_echo_server:acceptor ~n"),
  case gen_tcp:recv(Socket,0) of
	{ok,Packet} ->
	  error_logger:info_msg("echo >> ~p ~n",[Packet]),
	  gen_tcp:send(Socket,Packet),
	  acceptor(Socket);
	{error,closed} ->
	  error_logger:info_msg("A client [~p ] has closed ~n",[Socket]),
	  ok;
	{error,Reason} ->
	  error_logger:info_msg("A client [~p ] has occours error,the reason is ~p ~n",[Socket,Reason])
  end.

