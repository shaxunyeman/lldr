-module(lldr_data_store).
-author("shaxunyeman@gmail.com").

-behavior(gen_server).

%%
%% callback of gen_server
%%
-export([init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([start_link/0,stop/0]).
-export([ensure_loaded/0]).
-export([add_user/2,get_user/1]).
-export([get_user_password/1,update_user_password/1]).
-export([get_user_data_path/1]).

%%-define(NOTEST,true).
-include_lib("eunit/include/eunit.hrl").

-include("include/lldr_data_store.hrl").

-define(TIMEOUT,5000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

create_tables() ->
  mnesia:create_table(user,[{access_mode,read_write},{attributes,record_info(fields,user)}]),
  mnesia:create_table(password,[{access_mode,read_write},{attributes,record_info(fields,password)}]),
  ok.

ensure_loaded() ->
  mnesia:wait_for_tables([user,password],?TIMEOUT).
  
stop() ->
  %%mnesia:stop(),
  %%unregister(lldr_data_store).
  gen_server:call(?MODULE,stop).

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

add_user(User,Password) when is_record(User,user) and is_list(Password) ->
  Ref = erlang:make_ref(),
  %%lldr_data_store!{write,user,{self(),Ref},{User,Password}},
  gen_server:cast(lldr_data_store,{write,user,{self(),Ref},{User,Password}}),
  receive_ret_code(Ref); 
add_user(Mail,Password) when is_list(Mail) and is_list(Password) ->
  User = #user{mail=Mail,sex=1,name=Mail,path=""},
  add_user(User,Password).

get_user(Mail) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  %%lldr_data_store!{read,user,{self(),Ref},Mail},
  gen_server:cast(lldr_data_store,{read,user,{self(),Ref},Mail}),
  receive_record(Ref).

get_user_password(Mail) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  %%lldr_data_store!{get_password,{self(),Ref},Mail},
  gen_server:cast(lldr_data_store,{get_password,{self(),Ref},Mail}),
  receive_record(Ref).

update_user_password({Mail,NewPassword}) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  %%lldr_data_store!{update_password,{self(),Ref},{Mail,NewPassword}},
  gen_server:cast(lldr_data_store,{update_password,{self(),Ref},{Mail,NewPassword}}),
  receive_ret_code(Ref).

get_user_data_path(Mail) when is_list(Mail) ->
  Ref = erlang:make_ref(),
  %%lldr_data_store!{get_user_data_path,{self(),Ref},Mail},
  gen_server:cast(lldr_data_store,{get_user_data_path,{self(),Ref},Mail}),
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
	  %%io:format("~p ~n",[R])
	  error_logger:info_msg("~p: ~p <~p> handle_cast failed,the reason is : ~p~n",[?MODULE,?LINE,self(),R])
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callback of gen_server													 %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
  case mnesia:start() of
	ok ->
	  create_tables(),
	  error_logger:info_msg("~p:~p <~p> create tables ~n",[?MODULE,?LINE,self()]),
	  {ok,[]};
	{error,Reason} ->
	  error_logger:error_msg("~p:~p <~p> start data store failed,the reason is ~p ~n",[?MODULE,?LINE,Reason]),
	  {stop,Reason}
  end.
    
handle_cast(Request,Status) ->
  case Request of
	{write,user,{Requestor,Ref},{User,Password}} ->
	  io:format("~p: ~p <~p> handle_cast: ~p~n",[?MODULE,?LINE,self(),Request]),
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
	  %%io:format("~p ~n",[R])
	  error_logger:info_msg("~p: ~p <~p> handle_cast failed,the reason is : ~p~n",[?MODULE,?LINE,self(),R])
  end,
  {noreply,Status}.

handle_call(Request,From,State) ->
  case Request of
	stop ->
	  error_logger:info_msg("~p:~p <~p> stop ~n",[?MODULE,?LINE,self()]),
	  %% refer to more information: http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3
	  gen_server:reply(From,ok),
	  {stop,normal,State};
	  %% or evaluate below command 
	  %%{stop,normal,ok,State};
	_Any ->  
	  {noreply,State}
  end.

handle_info(Info,State) -> 
  %%io:format("Info: ~p ~n",[Info]),
  %%error_logger:info_msg("Info: ~p ~n",[Info]),
  {noreply,State}.

terminate(Reason,_State) ->
  error_logger:info_msg("terminate: ~p ~n",[Reason]),
  mnesia:stop(),
  unregister(?MODULE).

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test case														  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_user_test_() ->
  User = #user{mail="liuliu@eyou.net",sex=1,name="liuliu",path="/home/data"},
  add_user(User,"12306"),
  ?_assertEqual("12306",get_user_password("liuliu@eyou.net")).













