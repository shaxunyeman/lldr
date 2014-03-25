-module(handle_protocol).

-include("base.hrl").

-export([auth/1,post/1,post_data/1]).
-export([get/1]).


auth(#client{type=T,version=V,username=U,password=P}) ->
	io:format("Type=~p,version=~p,username=~p,password=~p ~n",[T,V,U,P]),
	ok.

post(#post{id=Id,filename=FileName,directory=Dir}) ->
	io:format("Id=~p,FileName=~p,Directory=~p ~n",[Id,FileName,Dir]),
	ok.

post_data(#post_data{id=_Id,data_begin=B,data_end=E,value=V}=_D) ->
	io:format("Value = ~p [offset : ~p ~p]~n",[V,list_to_integer(B),list_to_integer(E)]),
	ok.

%%
%% successfull {ok,Length}
%% failed	   {error,ErrorCode}
%%
get(#get{id=Id,filename=F}) ->
	io:format("Get Id = ~p,FileName = ~p ~n",[Id,F]),
	{ok,9}.
