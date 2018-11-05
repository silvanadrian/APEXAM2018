-module(district_qc).

-export([territory/0, setup_territory/1]).
-export([prop_activate/0, prop_take_action/0]).

-include_lib("eqc/include/eqc.hrl").

% use atoms with chars from a to z
atom() ->
    ?LET(S, eqc_gen:list(eqc_gen:choose(97, 122)), list_to_atom(S)).

territory() ->
    eqc_gen:map(eqc_gen:int(), {atom(), eqc_gen:int()}).

setup_territory(_) ->
    undefined.

prop_activate() ->
    false.

prop_take_action() ->
    false.
