-module(download_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% -----------------------------------------------------------
%%
%%% -----------------------------------------------------------

init(_) ->
    Info = #{exchange => <<"eaglebit_file_server">>,
	     queue => <<"q1234">>,
	     routing_key => <<"black">>},

    {ok, C} = kiks_consumer_sup:add_child(Info),
    kiks_consumer:add_listener(C, self()),

    {ok, []}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(What, State) ->
    io:fwrite("DOWNLOAD SERVER: ~p~n", [What]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
