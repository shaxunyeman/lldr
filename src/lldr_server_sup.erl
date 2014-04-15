-module(lldr_server_sup).
%% -behavior(gen_server).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

%% -export([start_link/0]).
%% -export([auth/2]).

%%
%% callback of gen_server
%%
%% -export([init/1]).
%% -export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%%start_link() ->
%%  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%%auth(UserName,Password) ->
%%  gen_server:call(?MODULE,{auth,{UserName,Password}}).

%%init(_Args) ->
%%  State = [],
%%  {ok,State}.

%%handle_call(Request,From,State) ->
%%  io:format("Request: ~p~nFrom: ~p ~n",[Request,From]),
%%  {reply,{auth,ok},State}.

%%handle_cast(Request,State) ->
%%  io:format("Request: ~p ~n",[Request]),
%%  {noreply,State}.

%%handle_info(Info,State) -> 
%%  io:format("Info: ~p ~n",[Info]),
%%  {noreply,State}.

%%terminate(Reason,_State) ->
%%  io:format("terminate: ~p ~n",[Reason]).

%%code_change(_OldVsn,State,_Extra) ->
%%  {ok,State}.


start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Args) ->
  {ok,{
	{one_for_one,1,5},
	[
	  {lldr_real_server,{lldr_real_server,start_link,[]},transient,5000,worker,[lldr_real_server]}
	  ]
  }}.






















  
