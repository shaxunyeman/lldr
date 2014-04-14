-module(lldr_sup).
-behaviour(supervisor).


-export([start_link/1,terminate_child/1]).
-export([init/1]).

-type(config() :: tuple()).
-type(startlink_err() :: {already_started,pid()} | {shutdown,term()} | term()).
-type(startlink_ret() :: {ok,pid()} | ignore | {error,startlink_err()}).

-spec(start_link/1 :: (config()) -> startlink_ret()).
start_link(Config) ->
  supervisor:start_link({local,?MODULE},?MODULE,Config).

-spec(terminate_child/1 :: (pid()) -> ok | {error,any()}).
terminate_child(Pid) ->
  supervisor:terminate_child(?MODULE,Pid).

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
  
