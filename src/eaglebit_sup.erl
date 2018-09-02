%%%-------------------------------------------------------------------
%% @doc eaglebit top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eaglebit_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},

    DownloadServer = #{
      id => download_server,
      start => {download_server, start_link, []},
      shutdown => brutal_kill,
      type => worker
     },

    KiksAMQPSup = #{
      id => kiks_amqp_sup,
      start => {kiks_amqp_sup, start_link, []},
      shutdown => brutal_kill,
      type => supervisor
     },

    ChildSpecs = [KiksAMQPSup,
		  DownloadServer],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
