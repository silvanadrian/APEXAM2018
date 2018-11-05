-module(district_qc).

-export([territory/0, setup_territory/1]).
-export([prop_activate/0, prop_take_action/0]).

-include_lib("eqc/include/eqc.hrl").

s_gen() ->
    ?SUCHTHAT(X,list(eqc_gen:choose(97, 122)), length(X) > 0).

% use atoms with chars from a to z
atom() ->
    ?LET(S, list(eqc_gen:choose(97, 122)), list_to_atom(S)).

territory() ->
    eqc_gen:map(eqc_gen:int(), list({atom(), eqc_gen:int()})).

create_districts([], Result) -> lists:flatten(Result);
create_districts([{Key, Connections} | Districts], Result) ->
    {ok, Pid1} = district:create(Key),
    Connect = create_connections(Pid1, Connections, []),
    NewResult = lists:append([Pid1, Connect], Result),
    create_districts(Districts, NewResult).

create_connections(_Pid, [], Result) -> Result;
create_connections(Pid, [{Atom, To} | Connections], Result) ->
    {ok, Pid2} = district:create(To),
    district:connect(Pid, Atom, Pid2),
    NewResult = lists:append([Pid2], Result),
    create_connections(Pid, Connections, NewResult).

%%#{-4 => [{ejbdi,-1},{jennby,16}], 6 => [{fa,-12},{ta,-17},{keyj,-15}], 8 => [{w,-17}]}
%% create all district in a map and conec
setup_territory(Map) ->
    create_districts(maps:to_list(Map), []).

prop_activate() ->
    false.

prop_take_action() ->
    false.


