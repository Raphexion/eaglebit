-module(auth_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

-export([start_link/0,
	 add_client/2,
	 public_key/0,
	 encode/2]).

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

add_client(Id, PublicKey) ->
    gen_server:call(?SERVER, {add_client, Id, PublicKey}).

public_key() ->
    gen_server:call(?SERVER, public_key).

encode(Id, Msg) ->
    gen_server:call(?SERVER, {encode, Id, Msg}).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

-record(auth_record, {private={error, not_loaded},
		      public_key={error, not_loaded},
		      client_keys={error, not_created}}).

%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

init(_) ->
    {ok, #auth_record{}, 0}.

handle_call({add_client, Id, Key}, _From, Record=#auth_record{client_keys=ClientKeys}) ->
    {reply, {ok, key_added}, Record#auth_record{client_keys=ClientKeys#{ Id =>  Key}}};

handle_call(public_key, _From, Record=#auth_record{public_key=PublicKey}) ->
    {reply, {ok, PublicKey}, Record};

handle_call({encode, Id, Msg}, _From, Record=#auth_record{client_keys=ClientKeys}) ->
    {reply, encode_(Id, Msg, ClientKeys), Record}.


handle_cast(_What, Record) ->
    {noreply, Record}.

handle_info(timeout, Record) ->
    PrivateKey = load_private(),
    PublicKey = load_public_key(),
    ClientKeys = load_client_keys(),
    {noreply, Record#auth_record{private=PrivateKey,
				 public_key=PublicKey,
				 client_keys=#{}}};

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

load_private() ->
    Dir = code:priv_dir(eaglebit),
    {ok, FileData} = file:read_file(filename:join([Dir, "private.pem"])),
    [KeyData] = public_key:pem_decode(FileData),
    public_key:pem_entry_decode(KeyData).

load_public_key() ->
    Dir = code:priv_dir(eaglebit),
    {ok, FileData} = file:read_file(filename:join([Dir, "public.pem"])),
    FileData.

load_client_keys() ->
    Dir = code:priv_dir(eaglebit),
    ClientFolder = filename:join([Dir, "clients"]),
    {ok, Filenames} = file:list_dir(ClientFolder),
    ClientKeys = split_filenames(Filenames),
    io:fwrite("~p~n", [ClientKeys]).

split_filenames(Filenames) ->
    split_filenames(Filenames, []).

split_filenames([], Acc) ->
    Acc;

split_filenames([H|T], Acc) ->
    split_filenames(string:split(H, ".", all), T, Acc).

split_filenames([Id, "public", "pem"], T, Acc) ->
    split_filenames(T, [{Id, "public", "pem"}|Acc]);

split_filenames(_, T, Acc) ->
    split_filenames(T, Acc).


%% --------------------------------------------------------------
%%
%% --------------------------------------------------------------

encode_(Id, Msg, ClientKeys) when is_map(ClientKeys) ->
    encode_(Id, Msg, maps:find(Id, ClientKeys));

encode_(Id, _Msg, error) ->
    {error, {missing_id, Id}};

encode_(_, Msg, {ok, Key}) ->
    public_key:encrypt_public(Msg, Key).
