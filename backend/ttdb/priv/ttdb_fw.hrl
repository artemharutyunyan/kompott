%% --------------------------------------------------------------------------
%% fw.hrl: firmware module definitions
%% --------------------------------------------------------------------------

-include("include/ttdb.hrl").
%%
%% Constants

-define(RIAK_HOST, "127.0.0.1").
-define(RIAK_PORT, 8087).

%% Main buckets
-define(CUSTOMER_BUCKET, <<"tt_customer_bucket">>).
-define(DEVICE_BUCKET, <<"tt_device_bucket">>).
-define(PACKAGE_BUCKET, <<"tt_package_bucket">>).
-define(RELEASE_BUCKET, <<"tt_release_bucket">>).

%% Sets for defining ownership
-define(CUSTOMER_DEVICE_SET_BUCKET, {<<"sets">>, <<"tt_customer_devices_bucket">>}).
-define(DEVICE_PACKAGE_SET_BUCKET, {<<"sets">>, <<"tt_device_packages_bucket">>}).
-define(PACKAGE_RELEASE_SET_BUCKET, {<<"sets">>, <<"tt_package_releases_bucket">>}).

%% Auxiliary buckets
-define(DEVICE_ID_UUID_MAP_BUCKET, <<"tt_device_id_to_uuid_map_bucket">>).

%% Records 
-record(customer, {name, uuid, creation_time}).
-record(device, {name, uuid, customer_id, external_id, description, creation_time, update_time}).
-record(package, {name, uuid, creation_time, update_time, latest_release}).
-record(release, {name, uuid, creation_time, version, files}).

%% Corresponding proplist key names
-define(NAME_KEY, <<"name">>).
-define(DESC_KEY, <<"description">>).
-define(ID_KEY, <<"deviceId">>).
-define(UPDATE_KEY, <<"updateTime">>).
-define(CREATION_KEY, <<"creationTime">>).

%% State definitions
-record(fw_state, {db_connection, connected = false}).

%% Process name
-define(DAEMON_NAME, ttdbd).

%% Error definitions
-define(E_DB_CONN, no_db_connection).
-define(E_BAD_REQ, bad_request).
-define(E_ID_EXIST,id_exists).


