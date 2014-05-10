-module(lldr_real_server).
-author("shaxunyeman@gmail.com").
-behavior(gen_server).

-include("include/typedef.hrl").

-export([start_link/1]).
-export([auth/2]).

%%
%% callback of gen_server
%%
-export([init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


-define(Host,"localhost").

-type(port() :: integer()).

-spec(start_link/1 :: (port()) -> term() | {error,term()}).
start_link(_Port) ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

auth(UserName,Password) ->
  gen_server:call(?MODULE,{auth,{UserName,Password}}).

init(_Args) ->
  %%io:format("Args ~p ~n",[_Args]),
  case gen_tcp:listen(8000,[binary,{packet,0},{active,true}]) of
	{ok,Ls} ->
	  {ok,{listen,Ls}};  
	{error,Reason} ->
	  {error,Reason}
  end.

handle_call(Request,From,State) ->
  io:format("Request: ~p~nFrom: ~p ~n",[Request,From]),
  {reply,{auth,ok},State}.

handle_cast(Request,State) ->
  io:format("Request: ~p ~n",[Request]),
  {noreply,State}.

handle_info(Info,State) -> 
  io:format("Info: ~p ~n",[Info]),
  {noreply,State}.

terminate(Reason,_State) ->
  io:format("terminate: ~p ~n",[Reason]).

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.


