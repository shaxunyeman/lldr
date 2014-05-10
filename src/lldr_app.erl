-module(lldr_app).
-author("shaxunyeman@gmail.com").

-behavior(application).

%%
%% callback of application
%%
-export([start/2,start_phase/3,prep_stop/1,stop/1,config_change/3]).


start(StartType, StartArgs) ->
  error_logger:info_msg("~p:~p <~p> StartType = ~p,StartArgs = ~p ~n",
						[?MODULE,?LINE,self(),StartType,StartArgs]),
  lldr_sup:start_link(StartArgs).


start_phase(Phase, StartType, PhaseArgs) ->
  error_logger:info_msg("~p:~p <~p> not implemented, ignore ~n",
						[?MODULE,?LINE,self()]),
  ok.

prep_stop(State) ->  
  error_logger:info_msg("~p:~p <~p> not implemented, ignore ~n",
						[?MODULE,?LINE,self()]),
  State.

stop(State) ->
  error_logger:info_msg("~p:~p <~p> stop ~n",
						[?MODULE,?LINE,self()]).

config_change(Changed, New, Removed) ->
  error_logger:info_msg("~p:~p <~p> Changend: ~p ~n \t New: ~p ~n \t Removed: ~p ~n",
						[?MODULE,?LINE,self(),Changed,New,Removed]),
  ok.
