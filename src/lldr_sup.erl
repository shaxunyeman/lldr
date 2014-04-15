-module(lldr_sup).
-behaviour(supervisor).

-include("typedef.hrl").

-export([start_link/1,terminate_child/1,which_children/0]).
-export([init/1]).

%% -type(configitem() :: {atom(),term()}).
%% -type(config() :: [configitem()]).
%% -type(startlink_err() :: {already_started,pid()} | {shutdown,term()} | term()).
%% -type(startlink_ret() :: {ok,pid()} | ignore | {error,startlink_err()}).

-spec(start_link/1 :: (config()) -> startlink_ret()).
start_link(Config) ->
  supervisor:start_link({local,?MODULE},?MODULE,Config).

-spec(terminate_child/1 :: (pid()) -> ok | {error,any()}).
terminate_child(Pid) ->
  supervisor:terminate_child(?MODULE,Pid).

which_children() ->
  supervisor:which_children(?MODULE).

init(Config) ->
  case lists:keyfind(file,1,Config) of
	{file,ConfigFileName} ->
	  case file:consult(ConfigFileName) of
		{ok,[SupSpec]} ->
		  io:format("SupSpec : ~p ~n",[SupSpec]),
		  {ok,SupSpec}
	  end;
	false ->
	  ignore
  end.
  
