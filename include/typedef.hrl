%%
%% include hrl files
%%
-include("base.hrl").

%%
%% 
%%
-type(configitem() :: {atom(),term()}).
-type(config() :: [configitem()]).
-type(startlink_err() :: {already_started,pid()} | {shutdown,term()} | term()).
-type(startlink_ret() :: {ok,pid()} | ignore | {error,startlink_err()}).


%%
%% type record
%%
-type(client() :: #client{}).
-type(post() :: #post{}).
-type(post_data() :: #post_data{}).
-type(push_data() :: #push_data{}).
-type(createdir() :: #createdir{}).
-type(deletedir() :: #deletedir{}).
-type(modifydir() :: #modify{}).
-type(listdir() :: #listdir{}).
-type(listfile() :: #listfile{}).
-type(logout() :: #logout{}).
-type(normal_response() :: #normal_response{}).
-type(post_response() :: #post_response{}).
-type(get() :: #get{}).


