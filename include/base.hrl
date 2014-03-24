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

%%
%% Command
%%
-define(AUTH,"auth").
-define(POST,"post").
-define(GET,"get").

-define(ID,"id").
-define(VERSION,"version").
-define(USERNAME,"username").
-define(PASSWORD,"password").
-define(COMMAND,"command").
-define(FILENAME,"filename").
-define(DIR,"directory").
-define(DESC,"filedescription").
-define(POSTDATA,"postdata").
-define(POSTBEGIN,"begin").
-define(POSTEND,"end").
-define(POSTVALUE,"value").
-define(POSTLENGTH,"length").

%%
%% response of code
%%
-define(OK,0).
-define(SOCKET_ERROR,1).
-define(PWD_INVALID,2).
