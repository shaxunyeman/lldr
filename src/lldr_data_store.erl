-module(lldr_data_store).
-include("lldr_data_store.hrl").

-export([start/0,stop/0]).
-export([add_user/2,get_user/1]).

-define(TIMEOUT,5000).


start() ->
  case mnesia:start() of
	ok ->
	  register(lldr_data_store,spawn(fun() -> loop() end));
	{error,Reason} ->
	  io:format("lldr_data_store start failed.Reason is ~p ~n",[Reason])
  end.

stop() ->
  mnesia:stop().

add_user(User,Password) ->
  lldr_data_store!{write,user,self(),{User,Password}},
  receive
	ok ->
	  ok
  after ?TIMEOUT ->
	{error,timeout}
  end.

get_user(Mail) when is_list(Mail) ->
  lldr_data_store!{read,user,self(),Mail},
  receive
	{ok,Record} ->
	  {ok,Record};
	'transaction abort' ->
	  {error,'transaction abort'}
  after ?TIMEOUT ->
	{error,timeout}
  end.

%get_user_password(Id) when is_integer(Id) ->
%  case get_user(Id) of
%	{ok,Record} ->
%  end.


loop() ->
  receive
	{write,user,Requestor,{User,Password}} ->
	  write_user(Requestor,{User,Password});
	{read,user,Requestor,Mail} ->
	  read_user(Requestor,Mail);
	{error,R} ->
	  io:format("~p ~n",[R])
  end,
  loop().

write_user_impl(User,Password) ->
  mnesia:write(user,User),
  mnesia:write(password,#password{mail=User#user.mail,string=Password}),
  ok.	

write_user(Requestor,{User,Password}) when is_list(Password) ->
  mnesia:transication(fun() ->
						case write_user_impl(User,Password) of
						  ok ->
							Requestor!ok
						end
					  end).

read_user(Mail) ->
  [User] = mnesia:read(user,Mail),
  {ok,User}.

read_user(Requestor,Mail) when is_list(Mail) ->  
  mnesia:transication(fun() ->
						case read_user(Mail) of
						  {ok,User} ->
							Requestor!{ok,User}
						end
					  end).
