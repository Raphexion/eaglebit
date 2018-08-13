-module(eaglebit).

-export([do/0]).

do() ->
    Msg = <<"hello crypto world">>,
    decrypt(encrypt(Msg)).

encrypt(Msg) ->
    Dir = code:priv_dir(eaglebit),
    {ok, FileData} = file:read_file(filename:join([Dir, "private.pem"])),
    [KeyData] = public_key:pem_decode(FileData),
    SKey = public_key:pem_entry_decode(KeyData),
    public_key:encrypt_private(Msg, SKey).

decrypt(Msg) ->
    Dir = code:priv_dir(eaglebit),
    {ok, FileData} = file:read_file(filename:join([Dir, "public.pem"])),
    [KeyData] = public_key:pem_decode(FileData),
    SKey = public_key:pem_entry_decode(KeyData),
    public_key:decrypt_public(Msg, SKey).
