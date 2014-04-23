-module(handle_protocol).
-author("shaxunyeman@gmail.com").

-include_lib("kernel/include/file.hrl").

-include("base.hrl").

-export([auth/1,post/2,post_data/2]).
-export([get/2]).
-export([push_data/5]).


-type(client() :: #client{}).
-type(post() :: #post{}).
-type(postresult() :: term()).
-type(post_data() :: #post_data{}).
%%-type(filename() :: string() | binary()).
-type(iodevice() :: pid()).
-type(get() :: #get{}).

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
post_data(IoDevice,#post_data{id=_Id,data_begin=B,data_end=E,value=V}=_D)  when is_integer(B) and is_integer(E) ->
	io:format("Value = ~p [offset : ~p ~p]~n",[V,B,E]),
	file:pwrite(IoDevice,B,V).

%%
%% successfull {ok,{ref(),{size,Length}}}
%% failed	   {error,ErrorCode}
%%
-spec(get/2 :: (client(),get()) -> {ok,term()} | {error,any()}).
get(Client,#get{id=Id,filename=F}) ->
	io:format("Get Id = ~p,FileName = ~p ~n",[Id,F]),
	case lldr_data_store:get_user_data_path(Client#client.username) of
	  {ok,DataPath} ->
		FullFileName = DataPath ++ F,
		case filelib:is_file(FullFileName) of
		  true ->
			case file:read_file_info(FullFileName) of	
			  {ok,FileInfo} ->
				{ok,{term_to_binary(make_ref()),{size,FileInfo#file_info.size}}};
			  {error,Reason} ->
				{error,Reason}
			end;
		  false ->
			{error,'file not exist'}
		end; 
	  {error,Reason} ->
		{error,Reason}
	end.

-define(FIELDSIZE,length("\"filedescription:\",\"begin:\",\"end:\",\"value:\"")).	

%% -spec(push_data/3 :: (client(),binary(),get()) -> {ok,binary()}).	
push_data(Client,Indentify,Get,Status,{M,F}) when is_binary(Indentify) ->
  io:format("push data from ~p ~n",[Get#get.filename]),
  {ok,DataPath} = lldr_data_store:get_user_data_path(Client#client.username),
  FullFileName = DataPath ++ Get#get.filename,
  {socket,OutPutIo} = lists:keyfind(socket,1,Status),
  case file:open(FullFileName,[read]) of
	{ok,IoDevice} ->
	  %% 客户端与服务器之间最大传输单元
	  %% 为什么减size(Indentify) - 13，因为数据有个数据头
	  loop_read_file(IoDevice,0,Client#client.mtu - size(Indentify) - ?FIELDSIZE,Indentify,OutPutIo,{M,F});
	{error,Reason} ->
	  io:format("file open error. [~p] ~n",[Reason])
  end.

loop_read_file(IoDevice,Local,Nember,Indentify,OutPutIo,{M,F}) when is_integer(Local) and is_integer(Nember) ->
  case file:pread(IoDevice,Local,Nember) of
	{ok,Data} ->
	  Begin = Local,
	  End = Local + Nember,
	  Content = {struct,[{"filedescription",binary_to_list(Indentify)},
						{"begin",Begin},
						{"end",End - 1},
						{"value",Data}]},
	  io:format("~p",[Content]),
	  Term_to_Json = mochijson2:encode(Content),	 
	  Bin = list_to_binary(Term_to_Json),
	  M:F(Bin,OutPutIo),
	  loop_read_file(IoDevice,End,Nember,Indentify,OutPutIo,{M,F});
	eof ->
	  eof;
	{error,Reason} ->
	  io:format("read file error.[~p] ~n",[Reason])
  end.












