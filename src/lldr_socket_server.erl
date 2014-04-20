-module(lldr_socket_server).
-author("db.liu").
-behavior(gen_server).

-export([start/3]).

%%
%% callback of gen_server
%%
-export([init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%%
%% used internal by proc_lib:spawn 
%%
-export([socket_server_acceptor_loop/1]).

-type(registername() :: atom()).
-type(port() :: integer()).
-type(function() :: atom()).
-type(acceptor() :: {module(),function()}).

-record(socket_server_info,{
		servername,
		ip=any,
		port,
		acceptor,
		listensocket=nul}).

-define(TCP_OPTIONS,[binary,{packet,0},{active,false},{reuseaddr,true}]).

-spec(start/3 :: (registername(),port(),acceptor()) -> {ok,pid()} | ignore | {error,any()}).
start(RegisterName,Port,Acceptor) when is_integer(Port) ->
  ServerInfo = #socket_server_info{servername=RegisterName,port=Port,acceptor=Acceptor},
  gen_server:start_link({local,RegisterName},?MODULE,ServerInfo,[]).

init(ServerInfo) ->
  case gen_tcp:listen(ServerInfo#socket_server_info.port,?TCP_OPTIONS) of
	{ok,ListenSocket} ->
	  io:format("~p listen on ~p ~n",[ServerInfo#socket_server_info.servername,ServerInfo#socket_server_info.port]),
	  NewServerInfo = ServerInfo#socket_server_info{listensocket=ListenSocket},
	  {ok,socket_server_acceptor(NewServerInfo)};
	{error,Reason} ->
	  {stop,Reason}
  end.
  
handle_cast({accept,_Pid},ServerInfo) ->
  io:format("continue process socket_server_acceptor ~n"),
  {noreply,socket_server_acceptor(ServerInfo)}.

socket_server_acceptor(#socket_server_info{servername=Name,acceptor=Acceptor,listensocket=ListenSocket}=ServerInfo) ->  
  proc_lib:spawn(?MODULE,socket_server_acceptor_loop,[{Name,ListenSocket,Acceptor}]),
  ServerInfo.

socket_server_acceptor_loop({ServerName,ListenSocket,{Module,Fun}}) ->
  case gen_tcp:accept(ListenSocket) of
	{ok,ClientSocket} ->
	  io:format("A new client [~p] has conncted ~n",[ClientSocket]),
	  gen_server:cast(ServerName,{accept,self()}),
	  Module:Fun(ClientSocket);
	{error,Reason} ->
	  io:format("socket_server_acceptor_loop: gen_tcp:accept faile,reason is ~p ~n",[Reason]),
	  {stop,Reason}
  end.

handle_call(_Request,_From,State) ->
  {noreply,State}.

handle_info(Info,State) -> 
  io:format("Info: ~p ~n",[Info]),
  {noreply,State}.

terminate(Reason,_State) ->
  io:format("terminate: ~p ~n",[Reason]).

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

