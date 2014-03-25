-module(response).

-export([ok/1,error/2]).
-export([okindentify/3]).

-include("base.hrl").

ok(EventId) when is_integer(EventId) ->
  Ok = ?OK,
  Response = <<EventId:32,Ok:32>>,
  Response.


error(EventId,ErrorCode) when is_integer(EventId) and is_integer(ErrorCode) -> 
  Response = <<EventId:32,ErrorCode:32>>,
  Response.


okindentify(EventId,Code,Indentify) when is_integer(EventId) and is_integer(Code) and is_binary(Indentify) ->
  Response = <<EventId:32,Code:32,Indentify/binary>>,
  Response.
