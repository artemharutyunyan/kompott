%% --------------------------------------------------------------------------
%% ttdb_fw.erl: firmware module implementation
%% --------------------------------------------------------------------------
-module(ttdb_fw).
-behaviour(gen_server).

-include("priv/ttdb_fw.hrl").

%% interface callbacks
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal functions
-export([customer_add/2, release_add/5]).
-export([release_get/4]).

%%
%% interface callback implementations
start_link() ->
    gen_server:start({local, ?DAEMON_NAME}, ?MODULE, [], []).

%%
%% gen_server callback implementations
init(_) ->
    {ok, Pid} = riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT),
    {ok, #fw_state{connected = true, db_connection = Pid}}.

handle_call(_Command, _From, #fw_state{ connected = false} = State) ->
    {reply, {error, ?E_DB_CONN}, State};

%% customer add
handle_call({customer_add, #tt_customer{} = TtCustomer}, _From, #fw_state{} = State) ->
    Customer = #customer{name = TtCustomer#tt_customer.name},
    {reply, customer_add(Customer, State), State};

%% device add/get
handle_call({device_add, #tt_device{} = TtDevice}, _From, #fw_state{} = State) ->
    %% Map request parameters to internal data structures
    Customer = #customer{uuid = TtDevice#tt_device.customer},
    Device = #device{
                       description = TtDevice#tt_device.description,
                              name = TtDevice#tt_device.name,
                       external_id = TtDevice#tt_device.id
                    },
    {reply, device_add(Customer, Device, State), State};
handle_call({device_get, #tt_device{} = TtDevice}, _from, #fw_state{} = State) ->
    %% Map request parameters to internal data structures
    Customer = #customer{uuid = TtDevice#tt_device.customer},
    Device = #device{external_id = TtDevice#tt_device.id},
    {reply, device_get(Customer, Device, State), State};

%% package add/get
handle_call({package_add, #tt_package{} = TtPackage}, _From, #fw_state{} = State) ->
    %% Map request parameters to internal data structures
    Customer = #customer{uuid = TtPackage#tt_package.customer},
    Device = #device{external_id = TtPackage#tt_package.device},
    Package = #package{
                               name = TtPackage#tt_package.name,
                        description = TtPackage#tt_package.description
                      },
    {reply, package_add(Customer, Device, Package, State), State};
handle_call({package_get, #tt_package{} = TtPackage}, _From, #fw_state{} = State) ->
    %% Map request parameters to internal data structures
    Customer = #customer{uuid = TtPackage#tt_package.customer},
    Device = #device{external_id = TtPackage#tt_package.device},
    Package = #package{uuid = TtPackage#tt_package.id},
    {reply, package_get(Customer, Device, Package, State), State};

handle_call(Request, _From, State) ->
    lager:warning("Unexpected request ~p in ~p:handle_call", [Request, ?MODULE]),
    {reply, {error, ?E_BAD_REQ}, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(Message, State) ->
    lager:info("Info message: ~p", [Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:info("Terminating for reason ~p", [Reason]),
    %% TODO: close connection to riak
    ok.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.

%%
%% 'Public' functions (called directly from gen_server callbacks

%% Creates an entry in the customer bucket
customer_add(#customer{} = Customer, State) ->
    {ok, Pid} = get_riak_connection(State),
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    lager:info("Adding customer ~p (UUID: ~p).", [Customer#customer.name, UUID]),
    C = Customer#customer{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
    CustomerObj = riakc_obj:new(?CUSTOMER_BUCKET, customer_key(C), C),
    ok = riakc_pb_socket:put(Pid, CustomerObj).

%% Creates an entry in the device bucket and adds the device to
%% customer's device set.
device_add(#customer{} = Customer, #device{} = Device, State) ->
    {ok, Pid} = get_riak_connection(State),
    %% Make sure that customer/external_id combination is unique
    IdMap = device_external_id_map_key(Customer, Device),
    case riakc_pb_socket:get(Pid, ?DEVICE_ID_UUID_MAP_BUCKET, IdMap) of
        {error, notfound} ->
            %% Create a device and add it to the device bucket
            UUID = uuid:uuid_to_string(uuid:get_v4()),
            lager:info("Adding device ~p (UUID ~p).", [Device#device.name, UUID]),
            D = Device#device{uuid = UUID, creation_time = ttcommon:utc_timestamp(), customer_id = Customer#customer.uuid},
            %% Add device object to the device bucket
            DeviceObj = riakc_obj:new(?DEVICE_BUCKET, device_key(Customer, D), term_to_binary(D)),
            ok = riakc_pb_socket:put(Pid, DeviceObj),
            %% Add mapping between external device Id and internal UUID
            DeviceIdMapObj = riakc_obj:new(?DEVICE_ID_UUID_MAP_BUCKET, IdMap, UUID),
            ok = riakc_pb_socket:put(Pid, DeviceIdMapObj),
            %% Add a device to customer's set of devices
            ok = add_device_to_customer_set(Customer, D, State);
        {ok, UUIDObj} ->
            %% Device already exists
            Val = binary_to_term(riakc_obj:get_value(UUIDObj)),
            lager:error("Device with ID:~p/Customer:~p already exists (UUID: ~p). Failed to add device.",
                [Device#device.external_id, Customer#customer.uuid, Val]),
            {error, ?E_ID_EXIST}
    end.

%% Creates an entry in the package bucket and adds the packade to
%% device's package set
package_add(#customer{} = Customer, #device{} = Device, #package{} = Package, State) ->
    {ok, Pid} = get_riak_connection(State),
    %% Make sure device with the given ID exists
    IdMap = device_external_id_map_key(Customer, Device),
    case riakc_pb_socket:get(Pid, ?DEVICE_ID_UUID_MAP_BUCKET, IdMap) of
        {error, notfound} ->
            %% Device ID does not exist
            lager:error("Device with ID: ~p/Customer: ~p does not exist. Failed to add package.",
                [Device#device.external_id, Customer#customer.uuid]),
                {error, ?E_ID_WRONG};
        {ok, UUIDObj} ->
            DeviceUUID = binary_to_term(riakc_obj:get_value(UUIDObj)),
            %% Create a package and add it to the package bucket
            UUID  = uuid:uuid_to_string(uuid:get_v4()),
            lager:info("Adding package ~p (UUID ~p) for device ~p, customer ~p.",
                [Package#package.name, UUID, DeviceUUID, Customer#customer.uuid]),
            P = Package#package{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
            D = Device#device{uuid = DeviceUUID},
            %% Add package to packages bucket
            PackageObj = riakc_obj:new(?PACKAGE_BUCKET, package_key(Customer, D, P), P),
            ok = riakc_pb_socket:put(Pid, PackageObj),
            %% Add package to device's set of packages
            ok = add_package_to_device_set(Customer, D, P, State),
            {ok, UUID}
    end.

%% Creates an entry in the release bucket and adds the release
%% to package's release list
release_add(#customer{} = Customer, #device{} = Device, #package{} = Package, #release{} = Release, State) ->
    {ok, Pid} = get_riak_connection(State),
    %% Create a release and add it to the release bucket
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    lager:info("Adding release ~p (UUID ~p) for package ~p, device ~p, customer ~p.",
        [Release#release.name, UUID, Package#package.uuid, Device#device.uuid, Customer#customer.uuid]),
    R = Release#release{uuid = UUID, creation_time = ttcommon:utc_timestamp()},
    %% Add release to releases bucket
    ReleaseObj = riakc_obj:new(?RELEASE_BUCKET, release_key(Customer, Device, Package, R), R),
    ok = riakc_pb_socket:put(Pid, ReleaseObj),
    %% Add a release to package's set of releases
    ok = add_release_to_package_set(Customer, Device, Package, R, State).

%% Retrieves devices of a given customer
device_get(#customer{} = Customer, #device{external_id = undefined}, State) ->
    %% Device ID not provided. Fetch all the devices for a given customer.
    S = get_customer_device_set(Customer, State),
    {ok, set_element_retrieve(S, ?DEVICE_BUCKET, fun device_rec_to_proplist/1, State)};
device_get(#customer{} = Customer, #device{} = Device, State) ->
    {ok, Pid} = get_riak_connection(State),
    %% Map an external ID (from request) to the internal UUID of the device
    IdMap = device_external_id_map_key(Customer, Device),
    case riakc_pb_socket:get(Pid, ?DEVICE_ID_UUID_MAP_BUCKET, IdMap) of
        {error, notfound} ->
            %% Did not find mapping entry
            [];
        {ok, UUIDObj} ->
            %% Got the real UUID
            UUID = binary_to_term(riakc_obj:get_value(UUIDObj)),
            DeviceKey = device_key(Customer, Device#device{uuid = UUID}),
            %% Fetch device object and return
            case riakc_pb_socket:get(Pid, ?DEVICE_BUCKET, DeviceKey) of
                {error, notfound} ->
                    {ok, []};
                {ok, DeviceObj} ->
                    F = fun(#device{} = D) -> device_rec_to_proplist(D) end,
                    L = lists:map(F, [binary_to_term(riakc_obj:get_value(DeviceObj))]),
                    {ok, L}
            end
    end.

%% Retrieves the list of packages for a given customer/device combination
package_get(#customer{} = Customer, #device{} = Device, #package{uuid = undefined}, State) ->
    {ok, Pid} = get_riak_connection(State),
    %% Make sure device with the given ID exists
    IdMap = device_external_id_map_key(Customer, Device),
    case riakc_pb_socket:get(Pid, ?DEVICE_ID_UUID_MAP_BUCKET, IdMap) of
        {error, notfound} ->
            %% Device Id does not exist
            lager:error("Device with ID ~p/Customer: ~p does not exist. Failed to add package.",
                [Device#device.external_id, Customer#customer.uuid]),
            {error, ?E_ID_WRONG};
        {ok, UUIDObj} ->
            DeviceUUID = binary_to_term(riakc_obj:get_value(UUIDObj)),
            D = Device#device{uuid = DeviceUUID},
            S = get_device_package_set(Customer, D, State),
            {ok, set_element_retrieve(S, ?PACKAGE_BUCKET, fun package_rec_to_proplist/1, State)}
    end;
%% Retrieves the list of packages for a given customer/device combination
package_get(#customer{} = Customer, #device{} = Device, #package{} = Package, State) ->
    {ok, Pid} = get_riak_connection(State),
    %% Make sure device with the given ID exists
    IdMap = device_external_id_map_key(Customer, Device),
    case riakc_pb_socket:get(Pid, ?DEVICE_ID_UUID_MAP_BUCKET, IdMap) of
        {error, notfound} ->
            %% Device Id does not exist
            lager:error("Device with ID ~p/Customer: ~p does not exist. Failed to add package.",
                [Device#device.external_id, Customer#customer.uuid]),
            {error, ?E_ID_WRONG};
        {ok, UUIDObj} ->
            DeviceUUID = binary_to_term(riakc_obj:get_value(UUIDObj)),
            D = Device#device{uuid = DeviceUUID},
            PackageKey = package_key(Customer, D, Package),
            %% Fetch package object and return
            case riakc_pb_socket:get(Pid, ?PACKAGE_BUCKET, PackageKey) of
                {error, notfound} ->
                    {ok, []};
                {ok, PackageObj} ->
                    {ok, [package_rec_to_proplist(binary_to_term(riakc_obj:get_value(PackageObj)))]}
            end
    end.



%% Retrives the list of releases for a given customer/device/package combination
release_get(#customer{} = Customer, #device{} = Device, #package{} = Package, State) ->
    S = get_package_release_set(Customer, Device, Package, State),
    set_element_retrieve(S, ?RELEASE_BUCKET, release_rec_to_proplist, State).

%%
%% Internal functions
%%

%% Auxiliary function for adding device to customer's device bucket set
add_device_to_customer_set(#customer{} = Customer, #device{} = Device, State) ->
    {ok, Pid} = get_riak_connection(State),
    S = get_customer_device_set(Customer, State),
    Set = riakc_set:add_element(device_key(Customer, Device), S),
    Key = customer_device_set_key(Customer),
    ok = riakc_pb_socket:update_type(Pid, ?CUSTOMER_DEVICE_SET_BUCKET, Key, riakc_set:to_op(Set)).

%% Auxiliary function for adding a package to device's package set
add_package_to_device_set(#customer{} = Customer, #device{} = Device, #package{} = Package, State) ->
    {ok, Pid} = get_riak_connection(State),
    S = get_device_package_set(Customer, Device, State),
    Set = riakc_set:add_element(package_key(Customer, Device, Package), S),
    Key = device_package_set_key(Customer, Device),
    ok = riakc_pb_socket:update_type(Pid, ?DEVICE_PACKAGE_SET_BUCKET, Key, riakc_set:to_op(Set)).

%% Auxiliary function for adding a release to to package's release set
add_release_to_package_set(#customer{} = Customer, #device{} = Device, #package{} = Package, #release{} = Release, State) ->
    {ok, Pid} = get_riak_connection(State),
    S = get_package_release_set(Customer, Device, Package, State),
    Set = riakc_set:add_element(release_key(Customer, Device, Package, Release), S),
    Key = package_release_set_key(Customer, Device, Package),
    ok = riakc_pb_socket:update_type(Pid, ?PACKAGE_RELEASE_SET_BUCKET, Key, riakc_set:to_op(Set)).

%% Auxiliary function for retrieving the bucket set where package's releases are kept
get_package_release_set(#customer{} = Customer, #device{} = Device, #package{} = Package, State) ->
    {ok, Pid} = get_riak_connection(State),
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
get_device_package_set(#customer{} = Customer, #device{} = Device, State) ->
    {ok, Pid} = get_riak_connection(State),
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
get_customer_device_set(#customer{} = Customer, State) ->
    {ok, Pid} = get_riak_connection(State),
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
        E ->
            lager:info("fetch_type returned ~p", [E]),
            throw(unexpected_fetch_type_return)
    end.

%% Functions for generating key names
customer_key(#customer{} = Customer) ->
    list_to_binary(Customer#customer.uuid).

device_key(#customer{} = Customer, #device{} = Device) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid).

device_external_id_map_key(#customer{} = Customer, #device{} = Device) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.external_id).

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
set_element_retrieve(Set, Bucket, ConvFun, State) ->
    {ok, Pid} = get_riak_connection(State),
    F = fun(Key, List) ->
            {ok, Elem} = riakc_pb_socket:get(Pid, Bucket, Key),
            [binary_to_term(riakc_obj:get_value(Elem))| List]
    end,
    Devices = riakc_set:fold(F, [], Set),
    C = fun(D) -> ConvFun(D) end,
    lists:map(C, Devices).

%% Return riak connection handle
get_riak_connection(#fw_state{ db_connection = Pid}) ->
    {ok, Pid}.

%% Convert a device record to proplist
device_rec_to_proplist(Device) ->
    [{?NAME_KEY, format_value(Device#device.name)},
     {?DESC_KEY, format_value(Device#device.description)},
     {?DEVICE_ID_KEY, format_value(Device#device.external_id)},
     {?UPDATE_KEY, format_value(Device#device.update_time)},
     {?CREATION_KEY, format_value(Device#device.creation_time)}
    ].

%% Convert a package record to proplist
package_rec_to_proplist(Package) ->
    [{?NAME_KEY, format_value(Package#package.name)},
     {?DESC_KEY, format_value(Package#package.description)},
     {?PACKAGE_ID_KEY, format_value(Package#package.uuid)},
     {?DEVICE_ID_KEY, format_value(Package#package.device_id)},
     {?UPDATE_KEY, format_value(Package#package.update_time)},
     {?CREATION_KEY, format_value(Package#package.creation_time)}
    ].

%% Format output values
format_value(Elem) when is_atom(Elem) ->
    list_to_binary(atom_to_list(Elem));
format_value(Elem) when is_integer(Elem) ->
    Elem;
format_value(Elem) when is_binary(Elem) ->
    Elem;
format_value(Elem) ->
    list_to_binary(Elem).
