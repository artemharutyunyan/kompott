%% --------------------------------------------------------------------------
%%
%% fw.erl: firmware module implementation 
%%
%% --------------------------------------------------------------------------
-module(fw).
-include("priv/fw.hrl").

-export([customer_add/1]).

customer_add(#customer{} = _Customer) ->
   ok.

