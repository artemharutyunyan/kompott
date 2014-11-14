%% --------------------------------------------------------------------------
%%
%% fw.hrl: firmware module definitions
%%
%% --------------------------------------------------------------------------

%% Constants
-define(CUSTOMER_BUCKET, <<tt_customer>>).

%% Records
-record(customer, {name, uuid, date}).


