-module(auth_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

-export([start_link/0,
	 add_client/1,
	 public_key/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_client(PublicKey) ->
    gen_server:call(?SERVER, {add_client, PublicKey}).

public_key() ->
    gen_server:call(?SERVER, public_key).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

-record(auth_record, {private_key={error, not_loaded},
		      public_key={error, not_loaded},
		      client_keys=sets:new()}).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

init(_) ->
    {ok, #auth_record{}, 0}.

handle_call({add_client, Key}, _From, Record=#auth_record{client_keys=ClientKeys}) ->
    {reply, {ok, key_added}, Record#auth_record{client_keys=sets:add_element(Key, ClientKeys)}};

handle_call(public_key, _From, Record=#auth_record{public_key=PublicKey}) ->
    {reply, {ok, PublicKey}, Record}.

handle_cast(_What, Record) ->
    {noreply, Record}.

handle_info(timeout, Record) ->
    PrivateKey = load_private_key(),
    PublicKey = load_public_key(),
    {noreply, Record#auth_record{private_key=PrivateKey,
				 public_key=PublicKey}};

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:fwrite("Code Changed :)~n"),
    {ok, State}.

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

load_private_key() ->
    Dir = code:priv_dir(eaglebit),
    {ok, FileData} = file:read_file(filename:join([Dir, "private.pem"])),
    [KeyData] = public_key:pem_decode(FileData),
    public_key:pem_entry_decode(KeyData).

load_public_key() ->
    Dir = code:priv_dir(eaglebit),
    {ok, FileData} = file:read_file(filename:join([Dir, "public.pem"])),
    FileData.
