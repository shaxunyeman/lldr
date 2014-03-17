%%
%%
%%

-record(client,{
    type,       %% type of client
		version,    %% version of client
		username,   %%
		password}). %%

-record(post,{
				id,
				filename,
				directory}).

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

%%
%% response of code
%%
-define(OK,0).
-define(SOCKET_ERROR,1).
-define(PWD_INVALID,2).
