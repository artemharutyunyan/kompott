%% --------------------------------------------------------------------------
%% ttdb.hrl: Database module definitions
%% --------------------------------------------------------------------------

%% Records
-record(tt_package, {name, device, customer, description, id}).
-record(tt_device, {name, id, customer, description}).
-record(tt_customer, {name}).
