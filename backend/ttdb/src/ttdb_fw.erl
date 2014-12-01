%% --------------------------------------------------------------------------
%%
%% fw.erl: firmware module implementation
%%
%% --------------------------------------------------------------------------
-module(ttdb_fw).
-include("priv/ttdb_fw.hrl").

-export([customer_add/1, device_add/2, package_add/3, release_add/4]).
-export([device_get/1, package_get/2, release_get/3]).

%%
%% Public functions
%%

%% Creates an entry in the customer bucket
customer_add(#customer{} = Customer) ->
    {ok, Pid} = get_riak_connection(),
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    lager:info("Adding customer ~p (UUID: ~p).", [Customer#customer.name, UUID]),
    C = Customer#customer{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
    CustomerObj = riakc_obj:new(?CUSTOMER_BUCKET, customer_key(C), C),
    ok = riakc_pb_socket:put(Pid, CustomerObj).

%% Creates an entry in the device bucket and adds the device to
%% customer's device set.
device_add(#customer{} = Customer, #device{} = Device) ->
    {ok, Pid} = get_riak_connection(),
    %% Create a device and add it to the device bucket
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    lager:info("Adding device ~p (UUID ~p).", [Device#device.name, UUID]),
    D = Device#device{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
    %% Add device object to the device bucket
    DeviceObj = riakc_obj:new(?DEVICE_BUCKET, device_key(Customer, D), D),
    ok = riakc_pb_socket:put(Pid, DeviceObj),
    %% Add a device to customer's set of devices
    ok = add_device_to_customer_set(Customer, D).

%% Creates an entry in the package bucket and adds the packadg to
%% device's package set
package_add(#customer{} = Customer, #device{} = Device, #package{} = Package) ->
    {ok, Pid} = get_riak_connection(),
    %% Create a package and add it to the package bucket
    UUID  = uuid:uuid_to_string(uuid:get_v4()),
    lager:info("Adding package ~p (UUID ~p) for device ~p, customer ~p.",
        [Package#package.name, UUID, Device#device.uuid, Device#device.uuid]),
    P = Package#package{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
    %% Add package to packages bucket
    PackageObj = riakc_obj:new(?PACKAGE_BUCKET, package_key(Customer, Device, P), P),
    ok = riakc_pb_socket:put(Pid, PackageObj),
    %% Add package to device's set of packages
    ok = add_package_to_device_set(Customer, Device, P).

%% Creates an entry in the release bucket and adds the release
%% to package's release list
release_add(#customer{} = Customer, #device{} = Device, #package{} = Package, #release{} = Release) ->
    {ok, Pid} = get_riak_connection(),
    %% Create a release and add it to the release bucket
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    lager:info("Adding release ~p (UUID ~p) for package ~p, device ~p, customer ~p.",
        [Release#release.name, UUID, Package#package.uuid, Device#device.uuid, Customer#customer.uuid]),
    R = Release#release{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
    %% Add release to releases bucket
    ReleaseObj = riakc_obj:new(?RELEASE_BUCKET, release_key(Customer, Device, Package, R), R),
    ok = riakc_pb_socket:put(Pid, ReleaseObj),
    %% Add a release to package's set of releases
    ok = add_release_to_package_set(Customer, Device, Package, R).

%% Retrieves the list of devices for a given customer
device_get(#customer{} = Customer) ->
    S = get_customer_device_set(Customer),
    set_element_retrieve(S, ?DEVICE_BUCKET).

%% Retrieves the list of packages for a given customer/device combination
package_get(#customer{} = Customer, #device{} = Device) ->
    S = get_device_package_set(Customer, Device),
    set_element_retrieve(S, ?PACKAGE_BUCKET).

%% Retrives the list of releases for a given customer/device/package combination
release_get(#customer{} = Customer, #device{} = Device, #package{} = Package) ->
    S = get_package_release_set(Customer, Device, Package),
    set_element_retrieve(S, ?RELEASE_BUCKET).

%%
%% Internal functions
%%

%% Auxiliary function for adding device to customer's device bucket set
add_device_to_customer_set(#customer{} = Customer, #device{} = Device) ->
    {ok, Pid} = get_riak_connection(),
    S = get_customer_device_set(Customer),
    Set = riakc_set:add_element(device_key(Customer, Device), S),
    Key = customer_device_set_key(Customer),
    ok = riakc_pb_socket:update_type(Pid, ?CUSTOMER_DEVICE_SET_BUCKET, Key, riakc_set:to_op(Set)).

%% Auxiliary function for adding a package to device's package set
add_package_to_device_set(#customer{} = Customer, #device{} = Device, #package{} = Package) ->
    {ok, Pid} = get_riak_connection(),
    S = get_device_package_set(Customer, Device),
    Set = riakc_set:add_element(package_key(Customer, Device, Package), S),
    Key = device_package_set_key(Customer, Device),
    ok = riakc_pb_socket:update_type(Pid, ?DEVICE_PACKAGE_SET_BUCKET, Key, riakc_set:to_op(Set)).

%% Auxiliary function for adding a release to to package's release set
add_release_to_package_set(#customer{} = Customer, #device{} = Device, #package{} = Package, #release{} = Release) ->
    {ok, Pid} = get_riak_connection(),
    S = get_package_release_set(Customer, Device, Package),
    Set = riakc_set:add_element(release_key(Customer, Device, Package, Release), S),
    Key = package_release_set_key(Customer, Device, Package),
    ok = riakc_pb_socket:update_type(Pid, ?PACKAGE_RELEASE_SET_BUCKET, Key, riakc_set:to_op(Set)).

%% Auxiliary function for retrieving the bucket set where package's releases are kept
get_package_release_set(#customer{} = Customer, #device{} = Device, #package{} = Package) ->
    {ok, Pid} = get_riak_connection(),
    Key = package_release_set_key(Customer, Device, Package),
    %% Retrieve package's releases
    case riakc_pb_socket:fetch_type(Pid, ?PACKAGE_RELEASE_SET_BUCKET, Key) of
        {ok, Set} ->
            %% Set exists
            Set;
        {error, {notfound, set}} ->
            %% Set does not exist. Create an empty set an return it.
            lager:info("Creating release set for package ~p, device ~p, customer ~p",
                [Package#package.uuid, Device#device.uuid, Customer#customer.uuid]),
            riakc_set:new();
        _ ->
            throw(unexpected_fetch_type_return)
    end.

%% Auxiliary function for retrieving the bucket set where device's packages are kept
get_device_package_set(#customer{} = Customer, #device{} = Device) ->
    {ok, Pid} = get_riak_connection(),
    Key = device_package_set_key(Customer, Device),
    %% Retrieve device's set
    case riakc_pb_socket:fetch_type(Pid, ?DEVICE_PACKAGE_SET_BUCKET, Key) of
        {ok, Set} ->
            %% Set exists
            Set;
        {error, {notfound, set}} ->
            %% Set does not exist. Create an empty set and return it.
            lager:info("Creating release set for device ~p, customer ~p.",
                       [Device#device.uuid, Customer#customer.uuid]),
            riakc_set:new();
        _ ->
            throw(unexpected_fetch_type_return)
    end.

%% Auxiliary function for retrieving the bucket set where customer's devices are kept
get_customer_device_set(#customer{} = Customer) ->
    {ok, Pid} = get_riak_connection(),
    Key = customer_device_set_key(Customer),
    %% Retrieve customer's set
    case riakc_pb_socket:fetch_type(Pid, ?CUSTOMER_DEVICE_SET_BUCKET, Key) of
        {ok, Set} ->
            %% Set exists
            Set;
        {error, {notfound, set}} ->
            %% Set does not exist. Create an empty set and return
            lager:info("Creating bucket set for customer ~p.", [Customer#customer.uuid]),
            riakc_set:new();
        _ ->
            throw(unexpected_fetch_type_return)
    end.

get_riak_connection() ->
    riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT).

%% Functions for generating key names
customer_key(#customer{} = Customer) ->
    list_to_binary(Customer#customer.uuid).

device_key(#customer{} = Customer, #device{} = Device) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid).

package_key(#customer{} = Customer, #device{} = Device, #package{} = Package) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid ++ "_" ++ Package#package.uuid).

release_key(#customer{} = Customer, #device{} = Device, #package{} = Package, #release{} = Release) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid ++ "_" ++ Package#package.uuid ++ "_" ++ Release#release.uuid).

customer_device_set_key(#customer{} = Customer) ->
    list_to_binary(Customer#customer.uuid ++ "_device_set").

device_package_set_key(#customer{} = Customer, #device{} = Device) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid ++ "_package_set").

package_release_set_key(#customer{} = Customer, #device{} = Device, #package{} = Package) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid ++ "_" ++ Package#package.uuid ++ "_release_set").

%% Convert a riak client set to list
set_element_retrieve(Set, Bucket) ->
    {ok, Pid} = get_riak_connection(),
    F = fun(Key, List) ->
            {ok, Elem} = riakc_pb_socket:get(Pid, Bucket, Key),
            [binary_to_term(riakc_obj:get_value(Elem))| List]
    end,
    riakc_set:fold(F, [], Set).


