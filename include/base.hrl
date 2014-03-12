%%
%%
%%

-record(client,{
    type,       %% type of client
		version,    %% version of client
		username,   %%
		password}). %%

-define(AUTH,1).
-define(POST,2).
