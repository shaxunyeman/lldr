-module(handle_protocol).

-include("base.hrl").

-export([auth/1,post/1]).


auth(#client{type=T,version=V,username=U,password=P}) ->
	io:format("Type=~p,version=~p,username=~p,password=~p ~n",[T,V,U,P]),
	ok.

post(#post{id=Id,filename=FileName,directory=Dir}) ->
	io:format("Id=~p,FileName=~p,Directory=~p ~n",[Id,FileName,Dir]),
	ok.
