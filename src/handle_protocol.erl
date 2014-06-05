-module(handle_protocol).
-author("shaxunyeman@gmail.com").

-include_lib("kernel/include/file.hrl").

%%-include("include/base.hrl").
-include("include/typedef.hrl").

-export([auth/1,post/2,post_data/2,post_data_finish/1]).
-export([get/2]).
-export([push_data/5]).
-export([createdir/2,deletedir/2,modifydir/2,listdir/2]).
-export([logout/2]).

%% test
-export([set_dir_by_path/1]).
%% end test


%%-type(client() :: #client{}).
%%-type(post() :: #post{}).
%%-type(get() :: #get{}).
%%-type(post_data() :: #post_data{}).
%%-type(filename() :: string() | binary()).

-type(postresult() :: term()).
-type(iodevice() :: pid()).

-spec(auth/1 :: (client()) -> ok | {error,any()}).
auth(#client{type=T,version=V,username=U,password=P}) ->
	%%io:format("Type=~p,version=~p,username=~p,password=~p ~n",[T,V,U,P]),
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
post(Client,#post{id=_Id,crc=_Crc,filename=FileName,directory=Dir}) ->
	%%io:format("Id=~p,FileName=~p,Directory=~p ~n",[Id,FileName,Dir]),
	%% judge disk space
	case lldr_data_store:get_user_data_path(Client#client.username) of
	  {ok,DataPath} ->
		%%io:format("~p's data path to ~p ~n",[Client#client.username,DataPath]),
		UserFullPath = DataPath ++ "/" ++ Dir,
		IsExist = filelib:is_dir(UserFullPath),
		if
		  IsExist =:= false ->
			file:make_dir(UserFullPath);
		  IsExist =:= true ->
			void
		end,
		FullFileName = UserFullPath ++ "/" ++ FileName,
		case file:open(FullFileName,[write,binary]) of
		  {ok,IoDevice} ->
			{ok,{term_to_binary(make_ref()),{file_fd,IoDevice},{file,FullFileName}}};
		  {error,Reason} ->
			{error,Reason}
		end;
	  {error,Reason} ->
		%%io:format("get user path failed ~n"),
		error_logger:error_msg("~p:~p get user path failed ~n",[?MODULE,?LINE]),
		{error,Reason}
	end.

-spec(post_data/2 :: (iodevice(),post_data()) -> ok | {error,any()}).
post_data(IoDevice,#post_data{id=_Id,data_begin=B,data_end=E,value=V}=_D)  when is_integer(B) and is_integer(E) ->
  %error_logger:info_msg("~p:~p [offset : ~p ~p]~n",[?MODULE,?LINE,B,E]),
  file:pwrite(IoDevice,B,V).

-spec(post_data_finish/1 :: (iodevice()) -> ok | {error,any()}).
post_data_finish(IoDevice) ->
  file:close(IoDevice).	

%%
%% successfull {ok,{ref(),{size,Length}}}
%% failed	   {error,ErrorCode}
%%
-spec(get/2 :: (client(),get()) -> {ok,term()} | {error,any()}).
get(Client,#get{id=Id,filename=F}) ->
	case lldr_data_store:get_user_data_path(Client#client.username) of
	  {ok,DataPath} ->
		%%FullFileName = DataPath ++ F,
		File1 = string:concat(DataPath,"/"),
		FullFileName = string:concat(File1,F), 
		case filelib:is_file(FullFileName) of
		  true ->
			case file:read_file_info(FullFileName) of	
			  {ok,FileInfo} ->
				{ok,Data} = file:read_file(FullFileName),
				CRC32 = erlang:crc32(Data),
				RefBin = term_to_binary(make_ref()),
				Response = [{filedescription,binary_to_list(RefBin)},
							{crc32,CRC32},
							{size,FileInfo#file_info.size}],
				{ok,Response};
				%%{ok,{term_to_binary(make_ref()),{size,FileInfo#file_info.size}}};
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
push_data(Client,Indentify,Get,Status,{M,F}) when is_list(Indentify) ->
  push_data(Client,list_to_binary(Indentify),Get,Status,{M,F});
push_data(Client,Indentify,Get,Status,{M,F}) when is_binary(Indentify) ->
  %error_logger:info_msg("~p:~p push data from ~p ~n",[?MODULE,?LINE,Get#get.filename]),
  {ok,DataPath} = lldr_data_store:get_user_data_path(Client#client.username),
  FileName = string:concat(DataPath,"/"),
  FullFileName = string:concat(FileName,Get#get.filename),
  {socket,OutPutIo} = lists:keyfind(socket,1,Status),
  %%error_logger:info_msg("~p:~p push data from ~p ~n",[?MODULE,?LINE,FullFileName]),
  case file:open(FullFileName,[read]) of
	{ok,IoDevice} ->
	  %% 客户端与服务器之间最大传输单元
	  %% 为什么减size(Indentify) - 13，因为数据有个数据头
	  loop_read_file(IoDevice,0,Client#client.mtu - size(Indentify) - ?FIELDSIZE,Indentify,OutPutIo,{M,F});
	{error,Reason} ->
	  error_logger:error_msg("~p : ~p file open error. [~p] ~n",[?MODULE,?LINE,Reason])
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
	  %%io:format("~p",[Content]),
	  Term_to_Json = mochijson2:encode(Content),	 
	  Bin = list_to_binary(Term_to_Json),
	  M:F(Bin,OutPutIo),
	  loop_read_file(IoDevice,End,Nember,Indentify,OutPutIo,{M,F});
	eof ->
	  %%error_logger:info_msg("~p:~p push data from  eof ~n",[?MODULE,?LINE]),
	  eof;
	{error,Reason} ->
	  error_logger:error_msg("~p : ~p read file error.The reason is [~p] ~n",[?MODULE,?LINE,Reason])
  end.

-spec(createdir/2 :: (client(),createdir()) -> ok | {error,any()}).
createdir(Client,Directory) ->
  case lldr_data_store:get_user_data_path(Client#client.username) of
	{ok,DataPath} ->
	  %%NewPath = DataPath ++ Directory#createdir.directory,
	  case Directory#createdir.directory of
		"./" ->
		  NewPath = string:concat(DataPath,"/"),
		  set_dir_by_path(NewPath);
		AnyDir ->
		  NewPath = string:concat(DataPath,"/"),
		  NewPath2 = string:concat(NewPath,AnyDir),
		  set_dir_by_path(NewPath2)
	  end,
	  ok;
	{error,Reason} ->
	  %%error_logger:error_msg("~p : ~p get path of user failed,the reason is ~p",[?MODULE,?LINE,Reason]),
	  {error,Reason}
  end.	

set_dir_by_path(Path) ->
  PathList = string:tokens(Path,"/"), 
  %io:format("~p~n",[PathList]),
  set_dir_by_path(PathList,"").

set_dir_by_path([],Dir) ->
  Dir;
set_dir_by_path([SubPath|Reset],Dir) ->
  Path1 = string:concat(Dir,SubPath),
  Path2 = string:concat(Path1,"/"),
  case filelib:is_dir(Path2) of
	true ->
	  set_dir_by_path(Reset,Path2);
	false ->
	  file:make_dir(Path2),
	  set_dir_by_path(Reset,Path2)
  end.

-spec(deletedir/2 :: (client(),deletedir()) -> ok | {error,any()}).
deletedir(Client,DeleteDir) ->
  case lldr_data_store:get_user_data_path(Client#client.username) of
	{ok,DataPath} ->
	  Path1 = string:concat(DataPath,"/"),
	  DeletePath = string:concat(Path1,DeleteDir#deletedir.directory),
	  case file:del_dir(DeletePath) of
		ok ->
		  ok;
		{error,Reason} ->
		  {error,Reason} 
	  end;
	{error,Reason} ->
	  {error,Reason}  
  end.

-spec(modifydir/2 :: (client(),modifydir()) -> ok | {error,any()}).
modifydir(Client,ModifyDir) ->
  case lldr_data_store:get_user_data_path(Client#client.username) of
	{ok,DataPath} ->
	  RootPath = string:concat(DataPath,"/"),
	  OldPath = string:concat(RootPath,ModifyDir#modify.old),
	  NewPath = string:concat(RootPath,ModifyDir#modify.new),
	  case file:del_dir(OldPath) of
		ok ->
		  file:make_dir(NewPath),
		  ok;
		{error,Reason} ->
		  {error,Reason}
	  end;
	{error,Reason} ->
	  {error,Reason} 
  end.

listdir(Path) ->
  case file:list_dir_all(Path) of
	{ok,FileNames} ->
	  NewFileNames = [set_file_flag(Path,Item) || Item <- FileNames],
	  {ok,NewFileNames};
	{error,Reason} ->
	  {error,Reason}
  end.

-type(filename_all() :: string() | binary()).
-type(filenames() :: [filename_all()]).
-spec(listdir/2 :: (client(),listdir()) -> {ok,filenames()} | {error,any()}).
listdir(Client,ListDir) ->
  case lldr_data_store:get_user_data_path(Client#client.username) of
	{ok,DataPath} ->
	  RootPath = string:concat(DataPath,"/"),
	  case ListDir#listdir.directory of
		"./" ->
		  listdir(RootPath);
		AnyDir ->
		  Path = string:concat(RootPath,ListDir#listdir.directory),
		  listdir(Path)
	  end;
	  %case file:list_dir_all(Path) of
	  %	{ok,FileNames} ->
	  %	  NewFileNames = [set_file_flag(Path,Item) || Item <- FileNames],
	  %	  {ok,NewFileNames};
	  %	{error,Reason} ->
	  %	  {error,Reason}
	  %end;
	{error,Reason} ->
	  {error,Reason}
  end.

set_file_flag(Path,Item) ->
  Path1 = string:concat(Path,"/"),
  FileName = string:concat(Path1,Item),
  case filelib:is_dir(FileName) of
	true ->
	  {dir,Item};
	false ->
	  {file,Item}
  end.


-spec(logout/2 :: (client(),logout()) -> ok).
logout(_Client,_LogOut) ->
  ok.



