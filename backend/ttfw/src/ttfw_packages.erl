%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module for handling requests to packages/ endpoint.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ttfw_packages).

-include_lib("ttdb/include/ttdb.hrl").
-include("priv/ttfw.hrl").

%% Cowboy callbacks
-export([init/3]).
-export([rest_init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([malformed_request/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([packages_to_json/2]).
-export([packages_from_json/2]).

%%
%% Cowboy callbacks
init(_Transport, _Request, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Request, []) ->
    {ok, Request, #package_req_state{}}.

known_methods(Request, State) ->
    {[<<"GET">>, <<"POST">>], Request, State}.

allowed_methods(Request, State) ->
    {[<<"GET">>, <<"POST">>], Request, State}.

malformed_request(Request, State) ->
    try malformed_request_unsafe(Request, State) of
        Ret -> Ret
    catch
        C:E ->
            lager:warning("Got exception ~p:~p. Stacktrace ~p", [C, E, erlang:get_stacktrace()]),
            {true, Request, State}
    end.

is_authorized(Request, State) ->
    case ttdb:authorize_request(State#package_req_state.package_desc) of
        true -> {true, Request, State};
        _ -> {{false, ?TTFW_REALM}, Request, State}
    end.


content_types_provided(Request, State) ->
    {[
        {<<"application/json">>, packages_to_json}
     ],
     Request, State}.

content_types_accepted(Request, State) ->
    {[
        {<<"application/json">>, packages_from_json}
     ],
     Request, State}.

%% GET handler
packages_to_json(Request, State) ->
    PackageDesc = State#package_req_state.package_desc,
    % Query the database
    case ttdb:package_get(PackageDesc) of
        {ok, JSON} ->
            {jsx:encode(JSON), Request, State};
        E ->
            lager:warning("Could not retrieve package (~p) information. Got ~p", [PackageDesc, E]),
            Body = <<"\{\"result\":\"error\", \"message\": \"Could not retrieve package information.\"\}">>,
            {Body, Request, State}
    end.

%% POST handler
packages_from_json(Request, State) ->
    PackageDesc = State#package_req_state.package_desc,

    % Add package to the database
    case ttdb:package_add(PackageDesc) of
        {ok, UUID} ->
            lager:info("Added package ~p for customer ~p (ID: ~p)",
                [PackageDesc#tt_package.name, PackageDesc#tt_package.customer, UUID]),
            Body = <<"\{\"result\":\"ok\"\}">>,
            Request2 = cowboy_req:set_resp_body(Body, Request);
        {error, id_wrong}  ->
            lager:warning("Could not process add package ~p for customer ~p device ~p",
                [PackageDesc#tt_package.name, PackageDesc#tt_package.customer, PackageDesc#tt_package.device]),
            Body = <<"\{\"result\":\"error\", \"message\": \"Could not add package. Device ID invalid.\"\}">>,
            Request2 = cowboy_req:set_resp_body(Body, Request);
        E ->
            lager:error("Could not process add package ~p for customer ~p device ~p. Error is ~p",
                [PackageDesc#tt_package.name, PackageDesc#tt_package.customer, PackageDesc#tt_package.device, E]),
            Body = <<"\{\"result\":\"error\", \"message\": \"Could not process request.\"\}">>,
            Request2 = cowboy_req:set_resp_body(Body, Request)
    end,
    {true, Request2, State}.

%%
%% Internal functions

%% Validates GET request params. Throws an exception in case of an error.
validate_GET_request_params(Request, State) ->
    {PackageDesc, Request2, State2} = extract_GET_request_params(Request, State),
    F = fun(Elem) -> validate_request_params(Elem, PackageDesc) end,
    lists:map(F, get_input_desc()),
    {false, Request2, State2#package_req_state{package_desc = PackageDesc}}.

%% Extracts GET input parameters from request
extract_GET_request_params(Request, State) ->
    AppendParam = fun(ParamName, {#tt_package{} = D, R, S}) -> extract(ParamName, {D, R, S}) end,
    lists:foldl(AppendParam, {#tt_package{}, Request, State}, get_input_desc()).

%% Returns the list of input parameters for the GET request
get_input_desc() ->
    [{customer, mandatory}, {device, mandatory}, {id, optional}].

%% Validates POST request params. Throws an exception in case of an error.
validate_POST_request_params(Request, State) ->
    {PackageDesc, Request2, State2} = extract_POST_request_params(Request, State),
    F = fun(Elem) -> validate_request_params(Elem, PackageDesc) end,
    lists:map(F, post_input_desc()),
    {false, Request2, State2#package_req_state{package_desc = PackageDesc}}.

%% Extracts POST input parameters from request
extract_POST_request_params(Request, State) ->
    AppendParam = fun(ParamName, {#tt_package{} = D, R, S}) -> extract(ParamName, {D, R, S}) end,
    lists:foldl(AppendParam, {#tt_package{}, Request, State}, post_input_desc()).

%% Returns the list of input parameters for the POST request.
post_input_desc() ->
    [{customer, mandatory}, {name, mandatory}, {device, mandatory}, {description, mandatory}].

%% Extracts an individual parameter from request
extract({customer, _}, {#tt_package{} = PackageDesc, Request, State}) ->
    {Customer, _} = cowboy_req:binding(customer, Request),
    {PackageDesc#tt_package{ customer = binary_to_list(Customer)}, Request, State};
extract({device, _}, {#tt_package{} = PackageDesc, Request, State}) ->
    case cowboy_req:binding(deviceId, Request) of
        {undefined, _} -> {PackageDesc#tt_package{device = undefined}, Request, State};
        {DeviceId, _} -> {PackageDesc#tt_package{device = binary_to_list(DeviceId)}, Request, State}
    end;
extract({id, _}, {#tt_package{} = PackageDesc, Request, State}) ->
    case cowboy_req:binding(packageId, Request) of
        {undefined, _} -> {PackageDesc#tt_package{id = undefined}, Request, State};
        {PackageId, _} -> {PackageDesc#tt_package{id = binary_to_list(PackageId)}, Request, State}
    end;
extract({name, _}, {#tt_package{} = PackageDesc, Request, State}) ->
    {Value, Request2, State2} = extract_param_from_json(<<"name">>, Request, State),
    {PackageDesc#tt_package{name = Value}, Request2, State2};
extract({description, _}, {#tt_package{} = PackageDesc, Request, State}) ->
    {Value, Request2, State2} = extract_param_from_json(<<"description">>, Request, State),
    {PackageDesc#tt_package{description = Value}, Request2, State2}.

%% Extracts value of the given key from JSON body and stores parsed JSON
extract_param_from_json(Name, Request, #package_req_state{parsed_body = undefined} = State) ->
    {ok, Body, Request2} = cowboy_req:body(Request),
    JSON = jsx:decode(Body),
    Value = proplists:get_value(Name, JSON),
    {Value, Request2, State#package_req_state{parsed_body = JSON}};
extract_param_from_json(Name, Request, State) ->
    Value = proplists:get_value(Name, State#package_req_state.parsed_body),
    {Value, Request, State}.

%% Validate request parameters
validate_request_params({name, mandatory}, #tt_package{name = undefined}) ->
   throw(bad_request);
validate_request_params({customer, mandatory}, #tt_package{customer = undefined}) ->
   throw(bad_request);
validate_request_params({description, mandatory}, #tt_package{description = undefined}) ->
   throw(bad_request);
validate_request_params({device, mandatory}, #tt_package{device = undefined}) ->
    throw(bad_request);
validate_request_params(_, _) ->
    ok.

%% Unsafe version of malformed_request/2 callback. Throws an exception in case of error.
malformed_request_unsafe(Request, State) ->
    case cowboy_req:method(Request) of
        {<<"GET">>, Request2} -> validate_GET_request_params(Request2, State);
        {<<"POST">>, Request2} -> validate_POST_request_params(Request2, State)
    end.
