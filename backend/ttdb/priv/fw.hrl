%% --------------------------------------------------------------------------
%%
%% fw.hrl: firmware module definitions
%%
%% --------------------------------------------------------------------------

%% Constants
-define(RIAK_HOST, "127.0.0.1").
-define(RIAK_PORT, 8087).
-define(CUSTOMER_BUCKET, <<"tt_customer">>).

%% Records
-record(customer, {name, uuid, date}).


