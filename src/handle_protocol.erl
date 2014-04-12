-module(handle_protocol).

-include("base.hrl").

-export([auth/1,post/2,post_data/2]).
-export([get/1]).


-type(client() :: #client{}).
-type(post() :: #post{}).
-type(postresult() :: term()).
-type(post_data() :: #post_data{}).
%%-type(filename() :: string() | binary()).
-type(iodevice() :: pid()).

-spec(auth/1 :: (client()) -> ok | {error,any()}).
auth(#client{type=T,version=V,username=U,password=P}) ->
	io:format("Type=~p,version=~p,username=~p,password=~p ~n",[T,V,U,P]),
	case lldr_data_store:get_user(U) of
	  {ok,_User} ->
		case lldr_data_store:get_user_password(U) of
		  {ok,Password} ->   
			if 
			  Password =:= P ->
				ok;
			  Password =/= P ->
				{error,'password invalid'}
			end;
		  {error,_} ->
			{error,'get password failed'}
		end;
	  {error,_}	->
		{error,'user not exist'}
	end.

-spec(post/2 :: (client(),post()) -> {ok,postresult()}| {error,any()}).	
post(Client,#post{id=Id,crc=_Crc,filename=FileName,directory=Dir}) ->
	io:format("Id=~p,FileName=~p,Directory=~p ~n",[Id,FileName,Dir]),
	%% judge disk space
	case lldr_data_store:get_user_data_path(Client#client.username) of
	  {ok,DataPath} ->
		io:format("~p's data path to ~p ~n",[Client#client.username,DataPath]),
		UserFullPath = DataPath ++ "/" ++ Dir,
		IsExist = filelib:is_dir(UserFullPath),
		if
		  IsExist =:= false ->
			file:make_dir(UserFullPath);
		  IsExist =:= true ->
			void
		end,
		FullFileName = UserFullPath ++ "/" ++ FileName,
		case file:open(FullFileName,[write,read,raw,binary,append]) of
		  {ok,IoDevice} ->
			{ok,{term_to_binary(make_ref()),{file,IoDevice}}};
		  {error,Reason} ->
			{error,Reason}
		end;
	  {error,Reason} ->
		io:format("get user path failed ~n"),
		{error,Reason}
	end.

-spec(post_data/2 :: (iodevice(),post_data()) -> ok | {error,any()}).
post_data(IoDevice,#post_data{id=_Id,data_begin=B,data_end=E,value=V}=_D) ->
	io:format("Value = ~p [offset : ~p ~p]~n",[V,list_to_integer(B),list_to_integer(E)]),
	file:pwrite(IoDevice,list_to_integer(B),V).

%%
%% successfull {ok,Length}
%% failed	   {error,ErrorCode}
%%
get(#get{id=Id,filename=F}) ->
	io:format("Get Id = ~p,FileName = ~p ~n",[Id,F]),
	{ok,9}.
