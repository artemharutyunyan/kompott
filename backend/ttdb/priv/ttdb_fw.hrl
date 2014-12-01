%% --------------------------------------------------------------------------
%%
%% fw.hrl: firmware module definitions
%%
%% --------------------------------------------------------------------------

%%
%% Constants

-define(RIAK_HOST, "127.0.0.1").
-define(RIAK_PORT, 8087).

-define(CUSTOMER_BUCKET, <<"tt_customer">>).
-define(DEVICE_BUCKET, <<"tt_device">>).
-define(PACKAGE_BUCKET, <<"tt_package">>).
-define(RELEASE_BUCKET, <<"tt_release">>).

-define(CUSTOMER_DEVICE_SET_BUCKET, {<<"sets">>, <<"tt_customer_devices">>}).
-define(DEVICE_PACKAGE_SET_BUCKET, {<<"sets">>, <<"tt_device_packages">>}).
-define(PACKAGE_RELEASE_SET_BUCKET, {<<"sets">>, <<"tt_package_releases">>}).

%% Records
-record(customer, {name, uuid, creation_time}).
-record(device, {name, uuid, creation_time, update_time}).
-record(package, {name, uuid, creation_time, update_time, latest_release}).
-record(release, {name, uuid, creation_time, version, files}).

