-module(lldr_response).
-author("shaxunyeman@gmail.com").

-export([code/2,code/3]).

code(EventId,Code) ->
  Items = [{"id",EventId},{"code",Code}],
  Term = {struct,Items},
  Json = mochijson2:encode(Term),
  iolist_to_binary(Json).


code(EventId,Code,Opts) when is_integer(Code) and is_integer(EventId) and is_list(Opts) ->
  Items = [{"id",EventId},{"code",Code}],
  JsonItems = code_opts(Opts,Items),
  NewJosnItems = lists:reverse(JsonItems),
  Term = {struct,NewJosnItems},
  Json = mochijson2:encode(Term),
  iolist_to_binary(Json).


code_opts([{Key,Value}|Rest],List) ->
  code_opts(Rest,[{Key,Value}|List]); 
code_opts([],List) ->
  List.
  
