-module(parse_protocol).

-include("base.hrl").

-export([auth/1]).


auth(#client{type=T,version=V,username=U,password=P}) ->
	io:format("Type=~p,version=~p,username=~p,password=~p ~n",[T,V,U,P]),
	ok.


