%% --------------------------------------------------------------------------
%% ttdb.erl: Public interface of the DB library
%% --------------------------------------------------------------------------
-module(ttdb).

-include("priv/ttdb_fw.hrl").

%% Public functions
-export([device_add/1]).

device_add(#tt_device{} = Device) ->
    gen_server:call(?DAEMON_NAME, {device_add, Device}, infinity).
