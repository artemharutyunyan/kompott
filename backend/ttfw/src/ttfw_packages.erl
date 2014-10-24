%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module for handling requests to packages/ endpoint.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ttfw_packages).

%% Cowboy callbacks 
-export([init/3, handle/2, terminate/3]).

init(_Transport, Request, []) ->
  {ok, Request, undefined}.

handle(Request, State) ->
  {_Method, Request1} = cowboy_req:method(Request),
  {ok, Request2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"TT #ftw">>, Request1),
  {ok, Request2, State}.

terminate(_Reason, _Request, _State) ->
  ok.

