-module(district_tests).
-author("silvan").
-include_lib("eunit/include/eunit.hrl").

district_create_test() ->
  ?assertMatch({ok, _}, district:create("Panem")).

district_get_description_test() ->
  {ok, P} = district:create("Panem"),
  ?assertEqual({ok, "Panem"}, district:get_description(P)),
  district:activate(P),
  ?assertEqual({ok, "Panem"}, district:get_description(P)).

district_connect_districts_test() ->
  {A, B, C} = create_districts(),

  ?assertEqual(ok, district:connect(A, b, B)),
  district:connect(A, c, C),
  % Action c already exists in A
  ?assertEqual(active, district:activate(A)),
  ?assertMatch({error, _}, district:connect(A, c, C)).

district_connect2_districts_test() ->
  {A, B, C} = create_districts(),

  ?assertEqual(ok, district:connect(A, b, B)),
  district:shutdown(C, self()),
  %trying to connect to a terminated district
  ?assertMatch({error, _}, district:connect(A, c, C)).

district_active_test() ->
  {A, B, C} = create_districts(),

  district:connect(A, c, C),
  district:shutdown(C, self()),
  % Process C not alive anymore, so A can't be activated
  ?assertEqual(false, is_process_alive(C)),
  ?assertEqual(impossible, district:activate(A)),
  % B doesn't have any neighbors, so easily to be activated
  ?assertEqual(active, district:activate(B)).

district_active2_test() ->
  {A, _, C} = create_districts(),

  district:connect(A, c, C),
  % Activate C already, activate A later
  ?assertEqual(active, district:activate(C)),
  ?assertEqual(active, district:activate(A)).

district_options_test() ->
  {A, B, C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  ?assertEqual({ok, [b, c]}, district:options(A)),
  ?assertEqual({ok, []}, district:options(B)),
  ?assertEqual({ok, []}, district:options(C)).

district_enter_test() ->
  {A, B, C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  Bob = {make_ref(), #{}},
  % only can enter if district active
  ?assertMatch({error, _}, district:enter(A, Bob)),
  district:activate(A),
  ?assertEqual(ok, district:enter(A, Bob)).

dsitrict_take_action_test() ->
  {A, B, C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),

  {KatnissRef, _} = Katniss = {make_ref(), #{}},
  {PeetaRef, _} = {make_ref(), #{}},
  district:activate(A),
  ?assertEqual(ok, district:enter(A, Katniss)),
  %Action doesn't exist
  ?assertMatch({error, _}, district:take_action(A, KatnissRef, d)),
  % Katniss stays in A
  ?assertMatch({error, _}, district:enter(A, Katniss)),
  %Creature hasn't joined A District
  ?assertMatch({error, _}, district:take_action(A, PeetaRef, b)),
  ?assertMatch({ok, _}, district:take_action(A, KatnissRef, b)),
  % Katniss now not in District A anymore
  ?assertEqual(ok, district:enter(A, Katniss)),
  % But now in district B
  ?assertMatch({error, _}, district:enter(B, Katniss)),
  %try to move Katniss by action again to district begin
  ?assertMatch({error, _}, district:take_action(A, KatnissRef, b)),
  district:shutdown(B, self()),
  ?assertMatch({error, _}, district:take_action(A, KatnissRef, b)),
  %therefore Katniss is still in A
  ?assertMatch({error, _}, district:enter(A, Katniss)).

district_shutdown_test() ->
  {A, B, C} = create_districts(),

  % Process is available
  ?assertEqual(true, is_process_alive(A)),
  ?assertEqual(true, is_process_alive(B)),
  ?assertEqual(true, is_process_alive(C)),
  district:connect(A, b, B),
  district:connect(A, c, C),

  ?assertEqual(ok, district:shutdown(A, self())),
  % after shutdown undefined
  ?assertEqual(false, is_process_alive(A)),
  ?assertEqual(false, is_process_alive(B)),
  ?assertEqual(false, is_process_alive(C)).

district_shutdown2_test() ->
  {A, B, C} = create_districts(),

  % Process is available
  ?assertEqual(true, is_process_alive(A)),
  ?assertEqual(true, is_process_alive(B)),
  ?assertEqual(true, is_process_alive(C)),
  district:connect(A, b, B),
  district:connect(A, c, C),

  ?assertEqual(ok, district:shutdown(B, self())),
  % after shutdown undefined
  ?assertEqual(true, is_process_alive(A)),
  ?assertEqual(false, is_process_alive(B)),
  ?assertEqual(true, is_process_alive(C)),
  %since B already shutdown, no need to send it a shutdown message anymore
  ?assertEqual(ok, district:shutdown(A, self())),
  %every district should be shutdown now
  ?assertEqual(false, is_process_alive(A)),
  ?assertEqual(false, is_process_alive(B)),
  ?assertEqual(false, is_process_alive(C)).

district_shutdown_cycle_test() ->
  {A, B, _} = create_districts(),

  district:connect(A, b, B),
  district:connect(B, a, A),
  district:connect(A,a,A),
  %times out since cycle exists
  district:shutdown(A,self()),
  ?assertEqual(false, is_process_alive(A)),
  ?assertEqual(false, is_process_alive(B)).

district_active_cycle_test() ->
  {A, B, C} = create_districts(),

  district:connect(A, b, B),
  district:connect(B, a, A),
  district:connect(B, c, C),
  district:connect(C, c, C),
  district:activate(A),
  {Ref, _} = Katniss = {make_ref(), #{}},
  % all connected districts get active
  ?assertMatch(ok, district:enter(C, Katniss)),
  district:take_action(C,Ref,c).

increment_grade(_, {CreatureRef, Stats}, Creatures) ->
  #{grade := CurGrade} = Stats,
  NewGrade = CurGrade + 4,
  case NewGrade of
    12 -> get_grade(CreatureRef, Stats, 12, happy, Creatures);
    7 -> get_grade(CreatureRef, Stats, 7, okay, Creatures);
    2 ->  get_grade(CreatureRef, Stats, 2, okay, Creatures);
    Grade -> get_grade(CreatureRef, Stats, Grade, sad, Creatures)
  end.

get_grade(Ref, Stats, Grade, Mood, Creatures) ->
  {{Ref, Stats#{grade := Grade,mood:= Mood}}, Creatures}.

district_trigger_test() ->
  {A, B, C} = create_districts(),

  district:connect(A, b, B),
  district:connect(A, c, C),
  district:connect(C, a, A),
  district:connect(B, a, A),

  district:trigger(A, fun increment_grade/3),
  district:activate(A),
  {Ref, _Stats} = Silvan = {make_ref(), #{grade => 0, mood => sad}},
  district:enter(A, Silvan),
  ?assertMatch({ok, _}, district:take_action(A, Ref, b)),
  ?assertMatch({ok, _},district:take_action(B, Ref, a)),
  ?assertMatch({ok, _}, district:take_action(A, Ref, b)),
  ?assertMatch({ok, _}, district:take_action(B, Ref, a)),
  ?assertMatch({ok, _}, district:get_description(B)),
  %Moved Silvan 4 times between A and B
  ?assertMatch({error,_},district:enter(A,Silvan)).


create_districts() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  {A, B, C}.