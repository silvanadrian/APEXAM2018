-module(district).
-behaviour(gen_statem).
-export([create/1,
         get_description/1,
         connect/3,
         activate/1,
         options/1,
         enter/2,
         take_action/3,
         shutdown/2,
         trigger/2]).
%% Gen_statem callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
%State Functions
-export([under_configuration/3, active/3]).
-type passage() :: pid().
-type creature_ref() :: reference().
-type creature_stats() :: map().
-type creature() :: {creature_ref(), creature_stats()}.
-type trigger() :: fun((entering | leaving, creature(), [creature()])
                                                   -> {creature(), [creature()]}).


-spec create(string()) -> {ok, passage()} | {error, any()}.
create(Desc) ->
    gen_statem:start(?MODULE, Desc, []).

-spec get_description(passage()) -> {ok, string()} | {error, any()}.
get_description(District) ->
    gen_statem:call(District, get_description).

-spec connect(passage(), atom(), passage()) -> ok | {error, any()}.
connect(From, Action, To) ->
    gen_statem:call(From, {connect, Action, To}).

-spec activate(passage()) -> active | under_activation | impossible.
activate(District) ->
    gen_statem:call(District, activate).

-spec options(passage()) -> {ok, [atom()]} | none.
options(District) ->
    gen_statem:call(District, options).

-spec enter(passage(), creature()) -> ok | {error, any()}.
enter(District, Creature) ->
    gen_statem:call(District, {enter, Creature}).

-spec take_action(passage(), creature_ref(), atom()) -> {ok, passage()} | {error, any()}.
take_action(From, CRef, Action) ->
    gen_statem:call(From, {take_action, CRef, Action}).

-spec shutdown(passage(), pid()) -> ok.
shutdown(District, NextPlane) ->
    gen_statem:call(District, {shutdown, NextPlane}).

-spec trigger(passage(), trigger()) -> ok | {error, any()} | not_supported.
trigger(_, _) ->
    not_supported.


%% States

handle_event({call,From}, get_description, Data) ->
  case maps:is_key(description, Data) of
   true -> {keep_state, Data, {reply, From, {ok, maps:get(description, Data)}}};
   false ->  {error, "No Description"}
  end;
handle_event({call, From}, options, Data) ->
  {keep_state, Data, {reply, From, {ok, maps:keys(maps:get(connections,Data))}}};
% ignore all other unhandled events
handle_event({call, From}, activate_instantion, Data) ->
  {next_state, active, Data, {reply, From, ok}};
handle_event({call, From}, {run_action, CRef}, Data) ->
  case maps:is_key(CRef, maps:get(creatures, Data)) of
    true -> {keep_state, Data, {reply, From, {error, "Creature is already in this District"}}};
    false ->  NewCreatures = maps:put(CRef, empty, maps:get(creatures, Data)),
              NewData = maps:update(creatures,NewCreatures,Data),
              {keep_state, NewData, {reply, From, ok}}
  end;
handle_event({call, From}, {enter, _}, Data) ->
  {keep_state, Data, {reply, From, {error, "Can't enter in this state"}}};

handle_event({call, From}, {shutdown, NextPlane}, Data) ->
  gen_statem:call(NextPlane, {shutting_down, From, maps:to_list(maps:get(creatures, Data))}),
  broadcast_shutdown(maps:get(connections, Data), NextPlane),
  {stop_and_reply, normal, {reply, From, ok}};
% ignore all other unhandled events
handle_event(_EventType, _EventContent, Data) ->
  {keep_state, Data}.

under_configuration({call, From}, {connect, Action, To}, Data) ->
  case maps:is_key(Action, maps:get(connections,Data)) of
    false -> Connections = maps:put(Action, To, maps:get(connections, Data)),
            NewData = maps:update(connections, Connections, Data),
            {keep_state, NewData, {reply, From, ok}};
    true -> {keep_state, Data, {reply, From, {error, "Action already exists"}}}
  end;

under_configuration({call, From}, activate, Data) ->
    case broadcast_connection(maps:to_list(maps:get(connections, Data))) of
      active -> {next_state, active, Data, {reply, From, active}};
      _ -> {next_state, active, Data, {reply, From, impossible}}
    end;
%% General Event Handling for state under_configuration
under_configuration(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

active({call, From}, {enter, {Ref, Stats}}, Data) ->
  case maps:is_key(Ref, maps:get(creatures, Data)) of
    true -> {keep_state, Data, {reply, From, {error, "Creture is already in this District"}}};
    false ->  NewCreatures = maps:put(Ref, Stats, maps:get(creatures, Data)),
              NewData = maps:update(creatures, NewCreatures, Data),
              {keep_state, NewData, {reply, From, ok}}
  end;
active({call, From}, {take_action, CRef, Action}, Data) ->
  case maps:is_key(Action, maps:get(connections, Data)) of
    true ->
      case maps:is_key(CRef, maps:get(creatures, Data)) of
        false -> {keep_state, Data, {reply, From, {error, "Creature doesn't exist in this district"}}};
        true -> {NewData, To} = creature_leave(CRef, Action, Data),
                case NewData of
                  error -> {keep_state, Data, {reply, From, {error, To}}};
                  _ ->      {keep_state, NewData, {reply, From, ok}}
                end
      end;
    false -> {keep_state, Data, {reply, From, {error, "Action doesn't exist"}}}
  end;
%% Handle Calls to active
active(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

init(Desc) ->
  %% Set the initial state + data
  State = under_configuration, Data = #{description => Desc, connections => #{}, creatures => #{}},
  {ok, State, Data}.

callback_mode() -> state_functions.

%% Synchronous Call which should wait until each response
broadcast_shutdown([], NextPlane) -> ok;
broadcast_shutdown([{_Action, To} | Actions ], NextPlane) ->
  gen_statem:call(To, {shutdown, NextPlane}),
  broadcast_shutdown(Actions, NextPlane).

%% Synchronous Call which should wait until each response
broadcast_connection([]) -> active;
broadcast_connection([{_Action, To} | Actions ]) ->
  gen_statem:call(To, activate_instantion),
  broadcast_connection(Actions).

creature_leave(CRef, Action, Data) ->
  To = maps:get(Action, maps:get(connections, Data)),
  case gen_statem:call(To, {run_action, CRef}) of
    ok -> NewCreatures = maps:remove(CRef, maps:get(creatures, Data)),
          NewData = maps:update(creatures, NewCreatures, Data),
          Return = {NewData, To};
    {error, Reason} -> Return = {error, Reason}
  end,
  Return.

