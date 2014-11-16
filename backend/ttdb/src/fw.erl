%% --------------------------------------------------------------------------
%%
%% fw.erl: firmware module implementation
%%
%% --------------------------------------------------------------------------
-module(fw).
-include("priv/fw.hrl").

-export([customer_add/1, device_add/2]).



%%
%% Public functions
%%

%% Creates an entry in the customer bucket
customer_add(#customer{} = Customer) ->
   {ok, Pid} = get_riak_connection(),
   UUID = uuid:get_v4(),
   lager:info("Adding customer ~p (UUID: ~p).", [Customer#customer.name, uuid:uuid_to_string(UUID)]),
   C = Customer#customer{uuid = uuid:uuid_to_string(UUID), creation_time = ttcommon:utc_timestamp()},
   CustomerObj = riakc_obj:new(?CUSTOMER_BUCKET, customer_key(C), C),
   ok = riakc_pb_socket:put(Pid, CustomerObj).

%% Creates an entry in the device bucket and adds the device to
%% customer's device set.
device_add(#customer{} = Customer, #device{} = Device) ->
   {ok, Pid} = get_riak_connection(),
   %% Create device and add it to the device bucket
   UUID = uuid:get_v4(),
   lager:info("Adding device ~p (UUID ~p).", [Device#device.name, uuid:uuid_to_string(UUID)]),
   D = Device#device{uuid = uuid:uuid_to_string(UUID), creation_time = ttcommon:utc_timestamp()},
   %% Add device object to the device bucket
   DeviceObj = riakc_obj:new(?DEVICE_BUCKET, device_key(Customer, D), D),
   ok = riakc_pb_socket:put(Pid, DeviceObj),
   %% Add a device to customer's set of devices
   ok = add_device_to_customer_set(Customer, D).

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
         lager:info("Bucket set for customer ~p did not exist. Creating one.", [Customer#customer.uuid]),
         riakc_set:new();
      _ ->
          throw(unexpected_fetch_type_return)
   end.

get_riak_connection() ->
   riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT).

%% Functions for generating key names
customer_key(#customer{} = Customer) ->
   list_to_binary(uuid:uuid_to_string(Customer#customer.uuid)).

device_key(#customer{} = Customer, #device{} = Device) ->
    list_to_binary(Customer#customer.uuid ++ "_" ++ Device#device.uuid).

customer_device_set_key(#customer{} = Customer) ->
    list_to_binary(Customer#customer.uuid ++ "_device_set").

