%% --------------------------------------------------------------------------
%% ttdb.erl: Public interface of the DB library
%% --------------------------------------------------------------------------
-module(ttdb).

-include("priv/ttdb_fw.hrl").

%% Public functions
-export([authorize_request/1]).
-export([customer_add/1]).
-export([device_add/1, device_get/1]).
-export([package_add/1, package_get/1]).

customer_add(#tt_customer{} = Customer) ->
    gen_server:call(?DAEMON_NAME, {customer_add, Customer}, infinity).

device_add(#tt_device{} = Device) ->
    gen_server:call(?DAEMON_NAME, {device_add, Device}, infinity).

device_get(#tt_device{} = Device) ->
    gen_server:call(?DAEMON_NAME, {device_get, Device}, infinity).

package_add(#tt_package{} = Package) ->
    gen_server:call(?DAEMON_NAME, {package_add, Package}, infinity).

package_get(#tt_package{} = Package) ->
    gen_server:call(?DAEMON_NAME, {package_get, Package}, infinity).

authorize_request(_Device) ->
    true.
