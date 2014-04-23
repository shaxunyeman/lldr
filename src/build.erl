-module(build).
-author("shaxunyeman@gmail.com").
-export([start/0]).

start() ->
  case file:consult("./lldr_build.conf") of
	{ok,BuildConf} ->
	  {erlfiles,ErlFiles} = lists:keyfind(erlfiles,1,BuildConf),
	  {compileopt,Opts} = lists:keyfind(compileopt,1,BuildConf),
	  [compile:file(X,Opts)||X <- ErlFiles];
	{error,Reason} ->
	  {stop,Reason}
  end.
