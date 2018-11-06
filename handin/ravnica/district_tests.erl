-module(district_tests).
-author("silvan").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

district_create_test() ->
  ?assertMatch({ok, _},district:create("Panem")).

district_get_description_test() ->
  {ok, P} = district:create("Panem"),
  ?assertEqual({ok, "Panem"}, district:get_description(P)),
  district:activate(P),
  ?assertEqual({ok, "Panem"}, district:get_description(P)).

district_connect_districts_test() ->
  {A,B,C} = create_districts(),

  ?assertEqual(ok, district:connect(A, b, B)),
  district:connect(A, c, C),
  % Action c already exists in A
  ?assertEqual(active, district:activate(A)),
  ?assertMatch({error, _}, district:connect(A, c, C)).

district_active_test() ->
  {A,B,C} = create_districts(),

  district:connect(A, c, C),
  district:shutdown(C,self()),
  % Process C not alive anymore, so A can't be activated
  ?assertEqual(false, is_process_alive(C)),
  ?assertEqual(impossible, district:activate(A)),
  % B doesn't have any neighbors, so easily to be activated
  ?assertEqual(active, district:activate(B)).

district_active2_test() ->
  {A,_,C} = create_districts(),

  district:connect(A, c, C),
  % Activate C already, activate A later
  ?assertEqual(active, district:activate(C)),
  ?assertEqual(active, district:activate(A)).

district_options_test() ->
  {A,B,C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  ?assertEqual({ok, [b,c]}, district:options(A)),
  ?assertEqual({ok, []}, district:options(B)),
  ?assertEqual({ok, []}, district:options(C)).

district_enter_test() ->
  {A,B,C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  {BobRef, _} = Bob = {make_ref(), #{}},
  % only can enter if district active
  ?assertMatch({error, _}, district:enter(A,BobRef)),
  district:activate(A),
  ?assertEqual(ok, district:enter(A,Bob)).

dsitrict_take_action_test() ->
  {A,B,C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  {KatnissRef, _} = Katniss = {make_ref(), #{}},
  {PeetaRef, _} = {make_ref(), #{}},
  district:activate(A),
  ?assertEqual(ok, district:enter(A,Katniss)),
  %Action doesn't exist
  ?assertMatch({error, _}, district:take_action(A, KatnissRef, d)),
  % Katniss stays in A
  ?assertMatch({error, _}, district:enter(A,Katniss)),
  %Creature hasn't joined A District
  ?assertMatch({error, _}, district:take_action(A, PeetaRef, b)),
  ?assertMatch({ok, _}, district:take_action(A, KatnissRef, b)),
  % Katniss now not in District A anymore
  ?assertEqual(ok, district:enter(A, Katniss)),
  % But now in district B
  ?assertMatch({error, _}, district:enter(B, Katniss)).

cheers(_, _Creature, _Creatures) ->
  io:format("Cheeeeers!~n").

district_shutdown_test() ->
  {A,B,C} = create_districts(),

  % Process is available
  ?assertMatch([_ | _], process_info(A)),
  ?assertMatch([_ | _], process_info(B)),
  ?assertMatch([_ | _], process_info(C)),
  district:connect(A, b, B),
  district:connect(A, c, C),

  ?assertEqual(ok,district:shutdown(A, self())),
  % after shutdown undefined
  ?assertEqual(undefined, process_info(A)),
  ?assertEqual(undefined, process_info(B)),
  ?assertEqual(undefined, process_info(C)).

district_trigger_test() ->
  {A,B,C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  %%?assertEqual(ok, district:trigger(A, fun cheers/3)),
  district:trigger(A, fun cheers/3),
  district:activate(A),
  Katniss = {make_ref(), #{}},
  district:enter(A,Katniss).


create_districts() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  {A,B,C}.