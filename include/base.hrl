%3
%%
%%

-record(client,{
    type,       %% type of client
		version,    %% version of client
		mtu = 1492,	%%   
		username,   %%
		password}). %%

-record(post,{
				id,
				crc,
				len,
				filename,
				directory}).

-record(post_data,{
		id,
		description,
		data_begin,
		data_end,
		value}).

-record(get,{
        id,
        filename}).

-record(push_data,{
		filedescription,
		data_begin,
		data_end,
		value}).

-record(createdir,{
		id,
		directory}).

-record(deletedir,{
		id,
		directory}).

-record(modify,{
		id,
		old,
		new}).

-record(listdir,{
		id,
		directory}).

-record(listfile,{
		id,
		directory}).

-record(logout,{
		id,
		type
		}).

-record(normal_response,{
		id,
		code
		}).

-record(post_response,{
		id,
		code,
		filedescription}).

%%
%% Command
%%
-define(REG,"register").
-define(AUTH,"auth").
-define(POST,"post").
-define(POSTDATA,"postdata").
-define(GET,"get").
-define(TYPE,"type").
-define(MKDIR,"createdir").
-define(DELDIR,"deletedir").
-define(LISTDIR,"listdir").
-define(LISTFILE,"listfile").
-define(MODIFY,"modify").
-define(LOGOUT,"logout").
-define(UNKOWNCOMMAND,"unkowncommand").


-define(ID,"id").
-define(CODE,"code").
-define(CRC,"crc").
-define(VERSION,"version").
-define(USERNAME,"username").
-define(PASSWORD,"password").
-define(COMMAND,"command").
-define(FILENAME,"filename").
-define(DIR,"directory").
-define(DESC,"filedescription").
-define(POSTBEGIN,"begin").
-define(POSTEND,"end").
-define(POSTVALUE,"value").
-define(POSTLENGTH,"length").
-define(MODIFYOLD,"old").
-define(MODIFYNEW,"new").
-define(PARENT,"parent").

%%
%% response of code
%%
-define(OK,0).
-define(SOCKET_ERROR,1).
-define(PWD_INVALID,2).
-define(USER_NOT_EXIST,3).
-define(FILE_NOT_EXIST,4).
-define(MKDIR_ERROR,5).
-define(DELDIR_ERROR,6).
-define(LISTDIR_ERROR,7).
-define(ERROR,10).
-define(UNKOWN_ERROR,100).

