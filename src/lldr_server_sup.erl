-module(lldr_server_sup).
-author("shaxunyeman@gmail.com").
%% -behavior(gen_server).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Args) ->
  {ok,{
	{one_for_one,1,5},
	[
	  {echosrv,{lldr_socket_server,start,[echosrv,8001,{lldr_echo_server,acceptor}]},transient,5000,worker,[lldr_socket_server,lldr_echo_server]},
	  {lldr_handle_server,{lldr_socket_server,start,[lldr_handle_server,8000,{lldr_handle_server,acceptor}]},transient,5000,worker,[lldr_socket_server,lldr_handle_server]}
	  ]
  }}.






















  
