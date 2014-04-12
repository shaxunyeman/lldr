%3
%%
%%

-record(client,{
    type,       %% type of client
		version,    %% version of client
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


%%
%% Command
%%
-define(AUTH,"auth").
-define(POST,"post").
-define(POSTDATA,"postdata").
-define(GET,"get").
-define(MKDIR,"createdir").
-define(DELDIR,"deletedir").
-define(LISTDIR,"listdir").
-define(LISTFILE,"listfile").
-define(MODIFY,"modify").
-define(LOGOUT,"logout").


-define(ID,"id").
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

%%
%% response of code
%%
-define(OK,0).
-define(SOCKET_ERROR,1).
-define(PWD_INVALID,2).
