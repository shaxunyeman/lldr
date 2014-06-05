-module(lldr_client).
-author("shaxunyeman@gmail.com").

-include_lib("kernel/include/file.hrl"). %% include file_info fo record
-include("include/typedef.hrl").

-export([start/0,stop/0]).
-export([connect/1,logout/0,close/0]).
-export([auth/2,createdir/1,deletedir/1,listfiles/1,listdir/1,moddir/2]).
-export([get/1,post/1]).

client_ref() ->
  Ref = erlang:make_ref(),
  {self(),Ref}.

rpc({Pid,Ref},Command) ->
  ?MODULE!{{Pid,Ref},Command},
  receive
	{ok,Ref,Result} ->
	  {ok,Result};
	{error,Ref,Reason} ->
	  {ok,Reason}
  end.

rpc_n(Command) ->
  ?MODULE!{rpc_n,binary_to_list(Command)}.

start() ->
  State = [],
  Pid = spawn(fun() -> loop(State) end),
  register(?MODULE,Pid).

stop() ->
  rpc(client_ref(),stop).

connect({Host,Port}) when is_list(Host) and is_integer(Port) ->
  rpc(client_ref(),{connect,{Host,Port}}).

close() ->
  rpc(client_ref(),close).

logout() ->
  Command = "{\"command\":\"logout\"",
  Type = "\"type\":\"pc\"",
  Id = "\"id\":5}",
  Logout = string:join([Command,Type,Id],","),
  rpc(client_ref(),{logout,Logout}).
  
auth(UserName,PassWord) when is_list(UserName) and is_list(PassWord) ->
  Command = "{\"command\":\"auth\"",
  Id = "\"id\":1",
  Version = "\"version\":\"1.0.0\"",
  N1 = string:concat("\"",UserName),
  N2 = string:concat(N1,"\""),
  Name = string:concat("\"username\":",N2),
  P1 = string:concat("\"",PassWord),
  P2 = string:concat(P1,"\""),
  Pwd = string:concat("\"password\":",P2),
  End = string:concat(Pwd,"}"),
  Auth = string:join([Command,Id,Version,Name,End],","),
  rpc(client_ref(),{auth,Auth}).

listfiles(Directory) ->
  Command = "{\"command\":\"listfile\"",
  Id = "\"id\":2",
  D1 = string:concat("\"",Directory),
  D2 = string:concat(D1,"\""),
  Dir = string:concat("\"directory\":",D2),
  End = string:concat(Dir,"}"),
  ListFiles = string:join([Command,Id,End],","),
  rpc(client_ref(),{listfile,ListFiles}).

listdir(ParentDir) ->
  Command = "{\"command\":\"listdir\"",
  Id = "\"id\":3",
  D1 = string:concat("\"",ParentDir),
  D2 = string:concat(D1,"\""),
  Dir = string:concat("\"directory\":",D2),
  End = string:concat(Dir,"}"),
  ListDir = string:join([Command,Id,End],","),
  rpc(client_ref(),{listdir,ListDir}).

moddir(OldDir,NewDir) ->
  Command = "{\"command\":\"modify\"",
  Id = "\"id\":4",
  Old1 = string:concat("\"",OldDir),
  Old2 = string:concat(Old1,"\""),
  New1 = string:concat("\"",NewDir),
  New2 = string:concat(New1,"\""),
  Old = string:concat("\"old\":",Old2),
  New = string:concat("\"new\":",New2),
  End = string:concat(New,"}"),
  ModDir = string:join([Command,Id,Old,End],","),
  rpc(client_ref(),{modify,ModDir}).

get(FileName) ->
  Command = "{\"command\":\"get\"",
  Id = "\"id\":6",
  File1 = string:concat("\"",FileName),
  File2 = string:concat(File1,"\""),
  File3 = string:concat("\"filename\":",File2),
  End = string:concat(File3,"}"),
  Get = string:join([Command,Id,End],","),
  rpc(client_ref(),{get,Get}).

createdir(SubDir) ->
  Command = "{\"command\":\"createdir\"",
  Id = "\"id\":7",
  S1 = string:concat("\"",SubDir),
  S2 = string:concat(S1,"\""),
  Sub = string:concat("\"directory\":",S2),
  End = string:concat(Sub,"}"),
  CreateDir = string:join([Command,Id,End],","),
  rpc(client_ref(),{createdir,CreateDir}).

deletedir(Dir) ->
  Command = "{\"command\":\"deletedir\"",
  Id = "\"id\":8",
  S1 = string:concat("\"",Dir),
  S2 = string:concat(S1,"\""),
  Sub = string:concat("\"directory\":",S2),
  End = string:concat(Sub,"}"),
  DelDir = string:join([Command,Id,End],","),
  rpc(client_ref(),{deletedir,DelDir}).

%%
%% post begin
%%
generate_json_post_data(FileIndentify,Value,Begin,End) when is_integer(Begin) and is_integer(End) ->  
  Content = {struct,[{"command","postdata"},
					  {"filedescription",FileIndentify},
					  {"id",100},
					  {"begin",Begin},
					  {"end",End},
					  {"value",Value}]},
  Term_to_Json = mochijson2:encode(Content),
  Bin = list_to_binary(Term_to_Json),
  Bin.

push_data({FileDesc,IoOutput},Location,CurByteSize) when CurByteSize > 0 ->
  NeedRead = erlang:min(CurByteSize,1024),
  case file:pread(IoOutput,Location,NeedRead) of
  {ok,Data} ->
	Begin = Location,
	if
	  CurByteSize >= 1024 ->
		End = Location + 1024,
		NewCurByteSize = CurByteSize - 1024;
	  CurByteSize < 1024 ->
		End = Location + CurByteSize,
		NewCurByteSize = 0
	end,
	Json_Data = generate_json_post_data(FileDesc,Data,Begin,End - 1), 
	%rpc_n(Json_Data),
	rpc(client_ref(),{post_data,Json_Data}),
	%%io:format("~p:~p ~p ~n",[?MODULE,?LINE,Json_Data]),
	io:format("~p:~p b=~p e=~p n=~p ~n",[?MODULE,?LINE,Begin,End,NewCurByteSize]),
	push_data({FileDesc,IoOutput},End,NewCurByteSize);
  eof ->
	file:close(IoOutput);
  {error,Reason} ->
	error_logger:error_msg("~p:~p read file occurs error. The reason is ~p ~n",[?MODULE,?LINE,Reason]) 
  end;
push_data({_FileDesc,IoOutput},_Location,CurByteSize)  when CurByteSize == 0 ->
  file:close(IoOutput).
  

post({Dir,FileName}) ->
  BaseName = filename:basename(FileName),
  {ok,FileInfo} = file:read_file_info(FileName),
  Length = FileInfo#file_info.size,
  {ok,FileData} = file:read_file(FileName),
  Crc32 = erlang:crc32(FileData),

  PostRequest = {struct,[{"command","post"},
						  {"id",9},
						  {"crc",Crc32},
						  {"length",Length},
						  {"filename",BaseName},
						  {"directory",Dir}]},
  PostJson = mochijson2:encode(PostRequest),
  case rpc(client_ref(),{post,PostJson}) of
	{ok,{Code,FileDesc}} ->
	  {ok,IoDevice} = file:open(FileName,[read,binary]),    
	  push_data({FileDesc,IoDevice},0,Length);
	{error,Reason} ->
	  {error,Reason}
  end.
%%
%% post end
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_socket(State) ->
  case lists:keyfind(socket,1,State) of
	{socket,Socket} ->
	  Socket;
	false ->
	  false
  end.

do_connect(State,{{Pid,Ref},{connect,{Host,Port}}}) ->
  case gen_tcp:connect(Host,Port,[binary,{packet,0},{active,false}]) of
	{ok,Socket} ->
	  NewState = [{socket,Socket}|State],
	  Pid!{ok,Ref,connected},
	  loop(NewState);
	{error,Reason} ->
	  Pid!{error,Ref,Reason}
  end.

do_close(State,{{Pid,Ref},close}) ->
  case get_socket(State) of
	Socket ->
	  gen_tcp:close(Socket),
	  NewState = lists:keydelete(socket,1,State),
	  Pid!{ok,Ref,closed},
	  loop(NewState);
	false ->
	  Pid!{error,Ref,'ecose'}
  end.

do_stop(State,{{Pid,Ref},stop}) ->
Pid!{ok,Ref,stopped}. 

request(Socket,Data) ->
  case gen_tcp:send(Socket,Data) of
	ok ->
	  case gen_tcp:recv(Socket,0,5000) of
		{ok,Packet} ->
		  {ok,binary_to_list(Packet)};
		{error,Reason} ->
		  {error,Reason}
	  end;
	{error,Reason} ->
	  {error,Reason}  
  end.

do_logout(State,{{Pid,Ref},{logout,Logout}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,Logout) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'elogout'}
  end.

do_auth(State,{{Pid,Ref},{auth,Auth}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,Auth) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'eauth'}
  end,
  loop(State).

do_listfile(State,{{Pid,Ref},{listfile,ListFiles}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,ListFiles) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'elistfile'}
  end,
  loop(State).


do_listdir(State,{{Pid,Ref},{listdir,ListDir}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,ListDir) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'elistdir'}
  end,
  loop(State).

do_modify(State,{{Pid,Ref},{modify,ModDir}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,ModDir) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'emoddir'}
  end,
  loop(State).

loop_get_data(Socket,{Pid,Ref}) ->
  case gen_tcp:recv(Socket,0,100) of
	{ok,Packet} ->
	  error_logger:info_msg("~p ~n",[Packet]),
	  Pid!{ok,Ref,Packet},
	  loop_get_data(Socket,{Pid,Ref});
	{error,Reason} ->
	  Pid!{error,Ref,Reason}
  end.
  
do_get(State,{{Pid,Ref},{get,Get}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,Get) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result},
		  loop_get_data(Socket,{Pid,Ref});
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'emoddir'}
  end,
  loop(State).

do_createdir(State,{{Pid,Ref},{createdir,CreateDir}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,CreateDir) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'ecreatedir'}
  end,
  loop(State).

do_deletedir(State,{{Pid,Ref},{delete,DeleteDir}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,DeleteDir) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'ecreatedir'}
  end,
  loop(State).

do_post(State,{{Pid,Ref},{post,PostData}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,PostData) of
		{ok,Result} ->
		  {ok,PostFields} = lldr_protocol_json:parse(Result),
		  {ok,PostRecord} = lldr_protocol_json:post_response(PostFields),
		  Pid!{ok,Ref,{PostRecord#post_response.code,PostRecord#post_response.filedescription}};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'epost'}
  end,
  loop(State).

do_postdata(State,{{Pid,Ref},{post,PostPureData}}) ->
  case get_socket(State) of
	Socket ->
	  case request(Socket,PostPureData) of
		{ok,Result} ->
		  Pid!{ok,Ref,Result};
		{error,Reason} ->
		  Pid!{error,Ref,Reason}
	  end;
	false ->
	  Pid!{error,Ref,'epostdata'}
  end,
  loop(State).

do_rpc_n(State,Data) ->
  case get_socket(State) of
	Socket ->
	  gen_tcp:send(Socket,Data);
	false ->
	  void
  end,
  loop(State).

%%  
%% loop
%%
loop(State) ->
  io:format("~p:~p <~p> ~p ~n",[?MODULE,?LINE,self(),State]),
  receive
	{{Pid,Ref},{connect,{Host,Port}}} ->
	  do_connect(State,{{Pid,Ref},{connect,{Host,Port}}});
	{{Pid,Ref},{logout,Logout}} ->
	  do_logout(State,{{Pid,Ref},{logout,Logout}});
	{{Pid,Ref},close} ->
	  do_close(State,{{Pid,Ref},close});
	{{Pid,Ref},stop} ->
	  do_stop(State,{{Pid,Ref},stop});
	{{Pid,Ref},{auth,Auth}} ->
	  do_auth(State,{{Pid,Ref},{auth,Auth}});
	{{Pid,Ref},{listfile,ListFiles}} ->
	  do_listfile(State,{{Pid,Ref},{listfile,ListFiles}});
	{{Pid,Ref},{listdir,ListDir}} ->
	  do_listdir(State,{{Pid,Ref},{listdir,ListDir}});
	{{Pid,Ref},{modify,ModDir}} ->
	  do_modify(State,{{Pid,Ref},{modify,ModDir}});
	{{Pid,Ref},{get,Get}} ->
	  do_get(State,{{Pid,Ref},{get,Get}});
	{{Pid,Ref},{createdir,CreateDir}} ->
	  do_createdir(State,{{Pid,Ref},{createdir,CreateDir}});
	{{Pid,Ref},{deletedir,DeleteDir}} ->
	  do_deletedir(State,{{Pid,Ref},{delete,DeleteDir}});
	{{Pid,Ref},{post,PostData}} ->
	  do_post(State,{{Pid,Ref},{post,PostData}});
	{{Pid,Ref},{post_data,PostPureData}} ->
	  do_postdata(State,{{Pid,Ref},{post,PostPureData}});
	{rpc_n,Data} ->
	  do_rpc_n(State,Data);
	Any ->
	  io:format("~p:~p <~p> Don't implement ~p ~n",[?MODULE,?LINE,self(),Any])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
