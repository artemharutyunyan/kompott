%% --------------------------------------------------------------------------
%%
%% fw.erl: firmware module implementation 
%%
%% --------------------------------------------------------------------------
-module(fw).
-include("priv/fw.hrl").

-export([customer_add/1]).

customer_add(#customer{} = Customer) ->
   {ok, Pid} = get_riak_connection(),
   UUID = uuid:get_v4(),
   lager:info("Adding customer ~p (UUID: ~p).", [Customer#customer.name, uuid:uuid_to_string(UUID)]),
   C = Customer#customer{uuid = uuid:uuid_to_string(UUID), date = ttcommon:utc_timestamp()},
   CustomerObj = riakc_obj:new(?CUSTOMER_BUCKET, list_to_binary(uuid:uuid_to_string(UUID)), C),
   ok = riakc_pb_socket:put(Pid, CustomerObj).

get_riak_connection() ->
   riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT).
