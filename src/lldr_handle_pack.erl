-module(lldr_handle_pack).
-author("shaxunyeman@gmail.com").

-include("include/typedef.hrl").

-export([parse_binary/2]).

parse_binary(Binary,Status) ->
  {auth,Value} = lists:keyfind(auth,1,Status),
  if
	Value =:= false ->
	  handle_user_auth(Binary,Status);
	Value =:= true ->
	  handle_command(Binary,Status)
  end.

handle_user_auth(Binary,Status) ->
  case lldr_protocol_json:parse(Binary) of
	{ok,Fields} ->
	  {ok,Client} = lldr_protocol_json:auth(Fields),
	  case handle_protocol:auth(Client) of
		ok ->
		  NewStatus = lists:keyreplace(auth,1,Status,{auth,true}),
		  EventId = lldr_protocol_json:protocol_id(Fields),
		  Response = lldr_response:code(EventId,?OK),
		  {ok,Response,[{client,Client}|NewStatus]};
		{error,'password invalid'} ->
		  EventId = lldr_protocol_json:protocol_id(Fields),
		  Response = lldr_response:code(EventId,?PWD_INVALID),
		  {error,Response,Status};
		{error,'user not exist'} ->
		  EventId = lldr_protocol_json:protocol_id(Fields),
		  Response = lldr_response:code(EventId,?USER_NOT_EXIST),
		  {error,Response,Status};
		{error,_Any} ->
		  EventId = lldr_protocol_json:protocol_id(Fields),
		  Response = lldr_response:code(EventId,?UNKOWN_ERROR),
		  {error,Response,Status}
	  end
  end.

handle_command(Binary,Status) ->
  case lists:keyfind(client,1,Status) of
	{client,Client} ->
	  case lldr_protocol_json:parse(Binary) of
		{ok,Fields} ->
		  case lldr_protocol_json:command(Fields) of
			?POST ->
			  handle_post(Client,Fields,Status);
			?POSTDATA ->
			  handle_post_data(Client,Fields,Status);
			?GET ->
			  handle_get(Client,Fields,Status);
			?MKDIR ->
			  handle_createdir(Client,Fields,Status);
			?DELDIR ->
			  handle_deletedir(Client,Fields,Status);
			?MODIFY ->
			  handle_modifydir(Client,Fields,Status);
			?LISTDIR ->
			  handle_listdir(Client,Fields,Status);
			?LOGOUT ->
			  handle_logout(Client,Fields,Status);
			?UNKOWNCOMMAND ->
			  error_logger:error_msg("~p:~p handle a unkown command that called ~p ~n",[?MODULE,?LINE,lldr_protocol_json:command(Binary)])
		  end
	  end;  
	false ->
	  error_logger:error_msg("~p:~p keyfind client failed ~n",[?MODULE,?LINE])
  end.

  

handle_post(Client,Fields,Status) ->
  {ok,Post} = lldr_protocol_json:post(Fields),
  case handle_protocol:post(Client,Post) of
	{ok,{Indentify,{file_fd,IoDevice},{file,FullFileName}}} when is_binary(Indentify) ->
	  EventId = lldr_protocol_json:protocol_id(Fields),
	  StrIndentify = binary_to_list(Indentify),
	  Response = lldr_response:code(EventId,?OK,[{"filedescription",StrIndentify}]),
	  NewStatus = [{StrIndentify,[{cur_size,0},{len,Post#post.len},{file_fd,IoDevice},{file,FullFileName}]}|Status],
	  {ok,Response,NewStatus};
	{error,_Reason} ->
	  EventId = lldr_protocol_json:protocol_id(Fields),
	  Response = lldr_response:code(EventId,?ERROR),
	  {error,Response,Status}
  end.

handle_post_data(Client,#post_data{id=Id,data_begin=Begin,data_end=End} = PostData,Status) when (Begin < 0) or (End < 0) ->
  error_logger:error_msg("~p:~n handle_post_data failed,the reason is Begin less than zero or End less than zero ~n",[?MODULE,?LINE]),
  Response = lldr_response:code(Id,?ERROR),
  {error,Response,Status};
handle_post_data(Client,#post_data{id=Id,data_begin=Begin,data_end=End} = PostData,Status) when (End < Begin) ->
  error_logger:error_msg("~p:~n handle_post_data failed,the reason is End less than Begin ~n",[?MODULE,?LINE]),
  Response = lldr_response:code(Id,?ERROR),
  {error,Response,Status};
handle_post_data(Client,#post_data{id=EventId,description=Indentify,data_begin=DataBegin,data_end=DataEnd} = PostData,Status) ->
  case lists:keyfind(Indentify,1,Status) of
	{Indentify,Value} ->
	  case lists:keyfind(file_fd,1,Value) of
		{file_fd,IoDevice} ->
		  case handle_protocol:post_data(IoDevice,PostData) of
			ok ->
			  %% update cur_size & judge cur_size =:= len
			  {cur_size,CurSize} = lists:keyfind(cur_size,1,Value),
			  {len,Length} = lists:keyfind(len,1,Value),
			  if 
				CurSize =:= 0 ->
				  {file,FileName} = lists:keyfind(file,1,Value),
				  error_logger:info_msg("~p:~n The file [~p] will begin receive ~n",[?MODULE,?LINE,FileName]);
				CurSize > 0 ->
				  void
			  end,
			  DataEnd = PostData#post_data.data_end,
			  DataBegin = PostData#post_data.data_begin,
			  NewCurSize = CurSize + (DataEnd - DataBegin + 1),
			  if 
				NewCurSize < Length ->
				  NewValue = lists:keyreplace(cur_size,1,Value,{cur_size,NewCurSize}),
				  NewStatus = lists:keyreplace(Indentify,1,Status,{Indentify,NewValue}),
				  %%EventId = lldr_protocol_json:protocol_id(Fields),
				  Response = lldr_response:code(EventId,?OK),
				  {ok,Response,NewStatus};
				NewCurSize >= Length ->
				  handle_protocol:post_data_finish(IoDevice),
				  {file,FullFileName} = lists:keyfind(file,1,Value),
				  NewStatus = lists:keydelete(Indentify,1,Status),
				  %%EventId = lldr_protocol_json:protocol_id(Fields),
				  Response = lldr_response:code(EventId,?OK),
				  error_logger:info_msg("~p:~n The file [~p] has posted ~n",[?MODULE,?LINE,FullFileName]),
				  {ok,Response,NewStatus}
			  end;
			{error,Reason} ->
			  error_logger:error_msg("~p:~n handle_protocol:post_data failed,the reason is ~p ~n",[?MODULE,?LINE,Reason]),
			  Response = lldr_response:code(EventId,?ERROR),
			  {error,Response,Status}
		  end;
		false ->
		  error_logger:error_msg("~p:~p keyfind file_fd failed ~n",[?MODULE,?LINE])
	  end;
	false ->
	  error_logger:error_msg("~p:~p keyfind filedescription failed ~n",[?MODULE,?LINE])
  end;
handle_post_data(Client,Fields,Status) ->
  {ok,PostData} = lldr_protocol_json:post_data(Fields),
  handle_post_data(Client,PostData,Status).
%%handle_post_data(Client,Fields,Status) ->
%%  {ok,PostData} = lldr_protocol_json:post_data(Fields),
%%  Indentify = PostData#post_data.description,
%%  case lists:keyfind(Indentify,1,Status) of
%%	{Indentify,Value} ->
%%	  case lists:keyfind(file_fd,1,Value) of
%%		{file_fd,IoDevice} ->
%%		  case handle_protocol:post_data(IoDevice,PostData) of
%%			ok ->
%%			  %% update cur_size & judge cur_size =:= len
%%			  {cur_size,CurSize} = lists:keyfind(cur_size,1,Value),
%%			  {len,Length} = lists:keyfind(len,1,Value),
%%			  DataEnd = PostData#post_data.data_end,
%%			  DataBegin = PostData#post_data.data_begin,
%%			  NewCurSize = CurSize + (DataEnd - DataBegin + 1),
%%			  if 
%%				NewCurSize < Length ->
%%				  NewValue = lists:keyreplace(cur_size,1,Value,{cur_size,NewCurSize}),
%%				  NewStatus = lists:keyreplace(Indentify,1,Status,{Indentify,NewValue}),
%%				  EventId = lldr_protocol_json:protocol_id(Fields),
%%				  Response = lldr_response:code(EventId,?OK),
%%				  {ok,Response,NewStatus};
%%				NewCurSize >= Length ->
%%				  handle_protocol:post_data_finish(IoDevice),
%%				  {file,FullFileName} = lists:keyfind(file,1,Value),
%%				  NewStatus = lists:keydelete(Indentify,1,Status),
%%				  EventId = lldr_protocol_json:protocol_id(Fields),
%%				  Response = lldr_response:code(EventId,?OK),
%%				  error_logger:info_msg("~p:~n The file [~p] has posted ~n",[?MODULE,?LINE,FullFileName]),
%%				  {ok,Response,NewStatus}
%%			  end;
%%			{error,Reason} ->
%%			  EventId = lldr_protocol_json:protocol_id(Fields),
%%			  Response = lldr_response:code(EventId,?ERROR),
%%			  {error,Response,Status}
%%		  end;
%%		false ->
%%		  error_logger:error_msg("~p:~p keyfind file_fd failed ~n",[?MODULE,?LINE])
%%	  end;
%%	false ->
%%	  error_logger:error_msg("~p:~p keyfind filedescription failed ~n",[?MODULE,?LINE])
%%  end.

handle_get(Client,Fields,Status) ->
  {ok,Get} = lldr_protocol_json:get(Fields),  
  EventId = Get#get.id,
  case handle_protocol:get(Client,Get) of
	{ok,GetResponse} ->
	  case lists:keyfind(outputcb,1,Status) of
		{outputcb,{M,F}} ->
		  {socket,Socket} = lists:keyfind(socket,1,Status),
			Response = lldr_response:code(EventId,?OK,GetResponse), 
			M:F(Response,Socket),
			{filedescription,Desc} = lists:keyfind(filedescription,1,GetResponse),
			handle_protocol:push_data(Client,Desc,Get,Status,{M,F});
		false ->
		  Response = lldr_response:code(EventId,?UNKOWN_ERROR), 
		  {ok,Response,Status}
	  end;
	{error,'file not exist'} ->
	  error_logger:error_msg("~p : ~p handle get failed,the reason is file not exist",[?MODULE,?LINE]),
	  Response = lldr_response:code(EventId,?FILE_NOT_EXIST),
	  {error,Response,Status};
	{error,Reason} ->
	  error_logger:error_msg("~p : ~p handle get failed,the reason is ~p",[?MODULE,?LINE,Reason]),
	  Response = lldr_response:code(EventId,?UNKOWN_ERROR),
	  {error,Response,Status}
  end.

handle_createdir(Client,Fields,Status) ->
  {ok,CreateDir} = lldr_protocol_json:createdir(Fields),
  EventId = CreateDir#createdir.id,
  case handle_protocol:createdir(Client,CreateDir) of
	ok ->
	  Response = lldr_response:code(EventId,?OK),
	  {ok,Response,Status};
	{error,Reason} ->
	  error_logger:error_msg("~p : ~p create directory failed,the reason is ~p",[?MODULE,?LINE,Reason]),
	  Response = lldr_response:code(EventId,?MKDIR_ERROR),
	  {error,Response,Status}
  end.


handle_deletedir(Client,Fields,Status) ->
  {ok,DeleteDir} = lldr_protocol_json:deletedir(Fields),
  EventId = DeleteDir#deletedir.id,
  case handle_protocol:deletedir(Client,DeleteDir) of
	ok ->
	  Response = lldr_response:code(EventId,?OK),
	  {ok,Response,Status};
	{error,Reason} ->
	  error_logger:error_msg("~p : ~p delete directory failed,the reason is ~p",[?MODULE,?LINE,Reason]),
	  Response = lldr_response:code(EventId,?DELDIR_ERROR),
	  {error,Response,Status}
  end.


handle_modifydir(Client,Fields,Status) ->
  {ok,ModifyDir} = lldr_protocol_json:modifydir(Fields),
  EventId = ModifyDir#modify.id,
  case handle_protocol:modifydir(Client,ModifyDir) of
	ok ->
	  Response = lldr_response:code(EventId,?OK),
	  {ok,Response,Status};
	{error,Reason} ->
	  error_logger:error_msg("~p : ~p modify directory failed,the reason is ~p",[?MODULE,?LINE,Reason]),
	  Response = lldr_response:code(EventId,?DELDIR_ERROR),
	  {error,Response,Status}
  end.


handle_listdir(Client,Fields,Status) ->
  {ok,ListDir} = lldr_protocol_json:listdir(Fields),
  EventId = ListDir#listdir.id,
  case handle_protocol:listdir(Client,ListDir) of
	{ok,FileNames} ->
	  Response = lldr_response:code(EventId,?OK,[{"list",FileNames}]),
	  {ok,Response,Status};
	{error,Reason} ->
	  Response = lldr_response:code(EventId,?LISTDIR_ERROR),
	  {error,Response,Status}
  end.

handle_logout(Client,Fields,Status) ->
  {ok,LogOut} = lldr_protocol_json:logout(Fields),
  EventId = LogOut#logout.id,
  handle_protocol:logout(Client,LogOut),
  Response = lldr_response:code(EventId,?OK),
  NewStatus = lists:keydelete(client,1,Status),
  %%NewStatus2 = lists:keydelete(socket,1,NewStatus1),
  {ok,Response,NewStatus}.  











