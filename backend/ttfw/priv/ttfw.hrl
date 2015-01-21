%% --------------------------------------------------------------------------
%% ttfw.hrl: ttfw application definitions
%% --------------------------------------------------------------------------

-define(TTFW_REALM, <<"Basic realm=\"kompott.am\"">>).

-record(dev_req_state, {parsed_body, device_desc}).
-record(package_req_state, {parsed_body, package_desc}).
