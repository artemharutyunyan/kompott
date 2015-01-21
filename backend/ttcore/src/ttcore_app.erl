-module(ttcore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
                {"/v1/customers/:customer/devicetypes/[:deviceId]", ttfw_devices, []},
                {"/v1/customers/:customer/devicetypes/:deviceId/packages/[:packageId]", ttfw_packages, []}
              ]
        }
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    ttcore_sup:start_link().

stop(_State) ->
    ok.
