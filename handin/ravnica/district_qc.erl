-module(district_qc).

-export([territory/0, setup_territory/1]).
-export([prop_activate/0, prop_take_action/0]).

-include_lib("eqc/include/eqc.hrl").

s_gen() ->
    ?SUCHTHAT(X,list(eqc_gen:choose(97, 122)), length(X) > 0).

% use atoms with chars from a to z
atom() ->
    ?LET(S, s_gen(), list_to_atom(S)).

territory() ->
    eqc_gen:map(eqc_gen:int(), {atom(), eqc_gen:int()}).

create_districts([], Result) -> Result;
create_districts([{Key, {Atom, To}} | Districts], Result) ->
    {ok, Pid1} = district:create(Key),
    {ok, Pid2} = district:create(To),
    district:connect(Pid1, Atom, Pid2),
    NewResult = lists:append([Pid1,Pid2], Result),
    create_districts(Districts, NewResult).

%%#{-5 => {hbcx,-8}, 0 => {wjbnr,15}, 4 => {zeploa,13}, 6 => {vqpciz,-10},19 => {tblb,-13}}
%% create all district in a map and conec
setup_territory(Map) ->
    create_districts(maps:to_list(Map), []).

prop_activate() ->
    false.

prop_take_action() ->
    false.
