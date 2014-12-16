%% --------------------------------------------------------------------------
%% ttdb.erl: Public interface of the DB library
%% --------------------------------------------------------------------------
-module(ttdb).

-include("priv/ttdb_fw.hrl").

%% Public functions
-export([device_add/1, device_get/1]).
-export([customer_add/1]).

device_add(#tt_device{} = Device) ->
    gen_server:call(?DAEMON_NAME, {device_add, Device}, infinity).

device_get(#tt_device{} = Device) ->
    gen_server:call(?DAEMON_NAME, {device_get, Device}, infinity).

customer_add(#tt_customer{} = Customer) ->
    gen_server:call(?DAEMON_NAME, {customer_add, Customer}, infinity).
