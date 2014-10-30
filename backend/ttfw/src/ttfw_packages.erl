%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module for handling requests to packages/ endpoint.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ttfw_packages).

%% Cowboy callbacks 
-export([init/3, handle/2, terminate/3]).

init(_Transport, Request, []) ->
  {ok, Request, undefined}.

handle(Request, State) ->
  {Method, Request1} = cowboy_req:method(Request),
  {ok, Request2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"TT #ftw">>, Request1),
  {ok, Request2, State}.

terminate(_Reason, _Request, _State) ->
  ok.

%% Endpoint handlers
packages(<<GET>>, Req) ->
  {Status, Resp} = ttcore:format_cowboy_reply(fun() -> get_packages(Req) end),
  cowboy_req:reply(Status, Resp).

%% GET packages/
get_packages(Req) ->
    {ok, json, <<{}>>}.

