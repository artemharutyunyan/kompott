%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module for handling requests to devices/ endpoint.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ttfw_devices).

-include_lib("ttdb/include/ttdb.hrl").
-include("priv/ttfw.hrl").

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

content_types_accepted(Request, _) ->
    {[
        {<<"application/json">>, devices_from_json}
     ],
     Request, #dev_req_state{}}.

is_authorized(Request, State) ->
    {true, Request, State}.

%% GET handler
devices_to_json(Request, State) ->
    % Extract request parameters
    {DeviceDesc, Request2, State2} = extract_GET_request_params(Request, State),
    % Validate request parameters
    F = fun(Elem) -> validate_request_params(Elem, DeviceDesc) end,
    lists:map(F, get_input_desc()),

    % Query the database
    case ttdb:device_get(DeviceDesc) of
        {ok, JSON} ->
            {jsx:encode(JSON), Request2, State2};
        E ->
            lager:error("Could not retrieve device (~p) information. Got ~p", [DeviceDesc, E]),
            Body = <<"\{\"result\":\"error\", \"message\": \"Could not retrieve device information.\"\}">>,
            {Body, Request2, State2}
    end.

%% POST handler
devices_from_json(Request, State) ->
    % Extract request parameters
    {DeviceDesc, Request2, State2} = extract_POST_request_params(Request, State),
    % Validate request parameters
    F = fun(Elem) -> validate_request_params(Elem, DeviceDesc) end,
    lists:map(F, post_input_desc()),

    % Add device to the database
    case ttdb:device_add(DeviceDesc) of
        ok ->
            lager:info("Added device ~p for customer ~p (ID:~p)",
                [DeviceDesc#tt_device.name, DeviceDesc#tt_device.customer, DeviceDesc#tt_device.id]),
            Body = <<"\{\"result\":\"ok\"\}">>,
            Request3 = cowboy_req:set_resp_body(Body, Request2);
        {error, id_exists}  ->
            lager:error("Could not process add device ~p for customer ~p. Id already exists (ID:~p)",
                [DeviceDesc#tt_device.name, DeviceDesc#tt_device.customer, DeviceDesc#tt_device.id]),
            Body = <<"\{\"result\":\"error\", \"message\": \"Could not add device. ID already exists.\"\}">>,
            Request3 = cowboy_req:set_resp_body(Body, Request2)
    end,
    {true, Request3, State2}.

%%
%% Internal functions

%% Returns the list of input parameters for the POST request.
post_input_desc() ->
    [{customer, mandatory}, {id, mandatory}, {name, mandatory}, {description, mandatory}].

%% Returns the list of input parameters for the GET request
get_input_desc() ->
    [{customer, mandatory}, {id, optional}].

%% Extracts GET input parameters from request
extract_GET_request_params(Request, State) ->
    AppendParam = fun(ParamName, {#tt_device{} = D, R, S}) -> extract(ParamName, {D, R, S}) end,
    lists:foldl(AppendParam, {#tt_device{}, Request, State}, get_input_desc()).

%% Extracts POST input parameters from request
extract_POST_request_params(Request, State) ->
    AppendParam = fun(ParamName, {#tt_device{} = D, R, S}) -> extract(ParamName, {D, R, S}) end,
    lists:foldl(AppendParam, {#tt_device{}, Request, State}, post_input_desc()).

%% Extracts an individual parameter from request
extract({customer, _}, {#tt_device{} = DeviceDesc, Request, State}) ->
    {Customer, _} = cowboy_req:binding(customer, Request),
    {DeviceDesc#tt_device{ customer = binary_to_list(Customer)}, Request, State};
extract({id, _}, {#tt_device{} = DeviceDesc, Request, State}) ->
    case cowboy_req:binding(deviceId, Request) of
        {undefined, _} -> {DeviceDesc#tt_device{id = undefined}, Request, State};
        {DeviceId, _} -> {DeviceDesc#tt_device{id = binary_to_list(DeviceId)}, Request, State}
    end;
extract({name, _}, {#tt_device{} = DeviceDesc, Request, State}) ->
    {Value, Request2, State2} = extract_param_from_json(<<"name">>, Request, State),
    {DeviceDesc#tt_device{name = Value}, Request2, State2};
extract({description, _}, {#tt_device{} = DeviceDesc, Request, State}) ->
    {Value, Request2, State2} = extract_param_from_json(<<"description">>, Request, State),
    {DeviceDesc#tt_device{description = Value}, Request2, State2}.

%% Extracts value of the given key from JSON body and stores parsed JSON
extract_param_from_json(Name, Request, #dev_req_state{parsed_body = undefined} = State) ->
    {ok, Body, Request2} = cowboy_req:body(Request),
    JSON = jsx:decode(Body),
    Value = proplists:get_value(Name, JSON),
    {Value, Request2, State#dev_req_state{parsed_body = JSON}};
extract_param_from_json(Name, Request, State) ->
    Value = proplists:get_value(Name, State#dev_req_state.parsed_body),
    {Value, Request, State}.

%% Validate request parameters
validate_request_params({name, mandatory}, #tt_device{name = undefined}) ->
   throw(bad_request);
validate_request_params({customer, mandatory}, #tt_device{customer = undefined}) ->
   throw(bad_request);
validate_request_params({description, mandatory}, #tt_device{description = undefined}) ->
   throw(bad_request);
validate_request_params({id, mandatory}, #tt_device{id = undefined}) ->
   throw(bad_request);
validate_request_params(_, _) ->
    ok.
