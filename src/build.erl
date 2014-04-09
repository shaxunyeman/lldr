-module(build).
-export([start/0]).

start() ->
  ErlFile = [server,protocol,response,handle_protocol,lldr_data_store],
  [compile:file(X,[{i,"../include"},{outdir,"../ebin"}])||X <- ErlFile].
