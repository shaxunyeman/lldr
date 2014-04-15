%%
%%
%%

-type(configitem() :: {atom(),term()}).
-type(config() :: [configitem()]).
-type(startlink_err() :: {already_started,pid()} | {shutdown,term()} | term()).
-type(startlink_ret() :: {ok,pid()} | ignore | {error,startlink_err()}).


