%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module for handling requests to devices/ endpoint.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ttfw_devices).

%% Cowboy callbacks
-export([init/3]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).

-export([devices_to_json/2]).
-export([devices_from_json/2]).

%%
%% Cowboy callbacks
init(_Transport, _Request, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Request, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Request, State}.

known_methods(Request, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Request, State}.

content_types_provided(Request, State) ->
    {[
        {<<"application/json">>, devices_to_json}
     ],
     Request, State}.

content_types_accepted(Request, State) ->
    {[
        {<<"application/json">>, devices_from_json}
     ],
     Request, State}.

is_authorized(Request, State) ->
    {true, Request, State}.

devices_to_json(Request, State) ->
    Body = <<"\{\"result\":\"ok\"\}">>,
    {Body, Request, State}.

devices_from_json(Request, State) ->
    {Customer, _} = cowboy_req:binding(customer, Request),
    {DeviceId, _} = cowboy_req:binding(deviceId, Request),
    lager:info("Reached here. Customer is: ~p, deviceId is: ~p", [Customer, DeviceId]),
    Body = <<"\{\"result\":\"ok\"\}">>,
    Req2 = cowboy_req:set_resp_body(Body, Request),
    {true, Req2, State}.


