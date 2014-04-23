-module(lldr_data_store).
-author("shaxunyeman@gmail.com").

-include("lldr_data_store.hrl").

-export([start/0,stop/0]).
-export([add_user/2,get_user/1]).
-export([get_user_password/1,update_user_password/1]).
-export([get_user_data_path/1]).

-define(TIMEOUT,5000).


start() ->
  register(lldr_data_store,spawn(fun() -> start_data_store() end)).

create_tables() ->
  mnesia:create_table(user,[{access_mode,read_write},{attributes,record_info(fields,user)}]),
  mnesia:create_table(password,[{access_mode,read_write},{attributes,record_info(fields,password)}]),
  ok.
  
start_data_store() ->
  case mnesia:start() of
	ok ->
	  create_tables(), 
	  loop()
  end.

stop() ->
  mnesia:stop(),
  unregister(lldr_data_store).

receive_record(Ref) ->
  receive
	{ok,Ref,Record} ->
	  {ok,Record};
	{error,Ref,Reason} ->
	  {ok,Reason}
  after ?TIMEOUT ->
	{error,timeout}
  end.

receive_ret_code(Ref) ->
  receive
	{ok,Ref} ->
	  ok;
	{error,Ref,Reason} ->
	  {error,Reason}
  after ?TIMEOUT ->
	{error,timeout}
  end.

add_user(User,Password) when is_list(Password) ->
  Ref = erlang:make_ref(),
  lldr_data_store!{write,user,{self(),Ref},{User,Password}},
  receive_ret_code(Ref). 

get_user(Mail) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  lldr_data_store!{read,user,{self(),Ref},Mail},
  receive_record(Ref).

get_user_password(Mail) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  lldr_data_store!{get_password,{self(),Ref},Mail},
  receive_record(Ref).

update_user_password({Mail,NewPassword}) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  lldr_data_store!{update_password,{self(),Ref},{Mail,NewPassword}},
  receive_ret_code(Ref).

get_user_data_path(Mail) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  lldr_data_store!{get_user_data_path,{self(),Ref},Mail},
  receive_record(Ref).


loop() ->
  receive
	{write,user,{Requestor,Ref},{User,Password}} ->
	  write_user({Requestor,Ref},{User,Password});
	{read,user,{Requestor,Ref},Mail} ->
	  read_user({Requestor,Ref},Mail);
	{get_password,{Requestor,Ref},Mail} ->
	  get_password({Requestor,Ref},Mail);
	{update_password,{Requestor,Ref},{Mail,NewPassword}} ->
	  update_password({Requestor,Ref},{Mail,NewPassword});
	{get_user_data_path,{Requestor,Ref},Mail} ->
	  user_data_path({Requestor,Ref},Mail);
	{error,R} ->
	  io:format("~p ~n",[R])
  end,
  loop().


write_user({Requestor,Ref},{User,Password}) ->
  mnesia:transaction(fun() ->
						mnesia:write(User),
						mnesia:write(#password{mail=User#user.mail,string=Password}),
						Requestor!{ok,Ref}
  					  end).

read_user(Mail) ->
  [User] = mnesia:read(user,Mail),
  {ok,User}.

read_user({Requestor,Ref},Mail) when is_list(Mail) ->  
  mnesia:transaction(fun() ->
  						case read_user(Mail) of
  						  {ok,User} ->
  							Requestor!{ok,Ref,User};
  						  {error,Reason} ->
  							Requestor!{error,Ref,Reason}
  						end
  					  end).


get_password({Requestor,Ref},Mail) when is_list(Mail) ->
  mnesia:transaction(fun() ->
						case mnesia:read(password,Mail) of
						  'transaction abort' ->
							Requestor!{error,Ref,'transaction abort'};
						  [Record] ->
							Requestor!{ok,Ref,Record#password.string}
						end
					  end).

update_password({Requestor,Ref},{Mail,NewPassword}) ->
  mnesia:transaction(fun() ->
						case mnesia:write(#password{mail=Mail,string=NewPassword}) of
						  'transaction abort' ->
							Requestor!{error,Ref,'transaction abort'};
						  ok ->
							Requestor!{ok,Ref}
						end
					  end).

user_data_path({Requestor,Ref},Mail) when is_list(Mail) ->
  mnesia:transaction(fun() ->
						case mnesia:read(user,Mail) of
						  'transaction abort' ->
							Requestor!{error,Ref,'transaction abort'};
						  [Record] ->
							Requestor!{ok,Ref,Record#user.path}						
						end
					  end).

















