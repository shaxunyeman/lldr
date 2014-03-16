%%
%%
%%

-record(client,{
    type,       %% type of client
		version,    %% version of client
		username,   %%
		password}). %%

%%
%% Command
%%
-define(AUTH,1).
-define(POST,2).
-define(GET,3).

-define(VERSION,"version").
-define(USERNAME,"username").
-define(PASSWORD,"password").
-define(ID,"id").

%%
%% response of code
%%
-define(SOCKET_ERROR,1).
-define(PWD_INVALID,2).
