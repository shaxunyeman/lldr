-module(lldr_real_server).
-include("base.hrl").

-export([start_link/1]).
-export([init/1]).

-define(Host,"localhost").

-type(port() :: integer()).

-spec(start_link/1 :: (port()) -> term() | {error,term()}).
start_link(Port) ->
  proc_lib:start_link(?MODULE,init,Port).

init(Port) ->
  case gen_tcp:listen(Port,[binary,{packet,4},{active,false}]) of
	{ok,Ls} ->
	  accept(Ls);
	{error,Reason} ->
	  {error,Reason}
  end.

accept(Ls) ->
  case gen_tcp:access(Ls) of 
	{ok,ClientSocket} ->
	  spawn_client(ClientSocket),
	  accepts(Ls);
	{error,Reason} ->
	  {error,Reason}
  end.
