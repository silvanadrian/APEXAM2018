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
-export([under_configuration/3, active/3, shutting_down/3, under_activation/3]).
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
trigger(District, Trigger) ->
  gen_statem:call(District, {trigger, Trigger}).


%% States
handle_event({call, From}, get_description, Data) ->
  case maps:is_key(description, Data) of
    true -> {keep_state, Data, {reply, From, {ok, maps:get(description, Data)}}};
    false -> {error, "No Description"}
  end;

handle_event({call, From}, options, Data) ->
  {keep_state, Data, {reply, From, {ok, maps:keys(maps:get(connections, Data))}}};

% ignore all other unhandled events
handle_event({call, From}, activate, Data) ->
  {next_state, active, Data, {reply, From, ok}};

handle_event({call, From}, {run_action, CRef, Stats}, Data) ->
  case maps:is_key(CRef, maps:get(creatures, Data)) of
    true -> {keep_state, Data, {reply, From, {error, "Creature is already in this District"}}};
    false -> NewCreatures = maps:put(CRef, Stats, maps:get(creatures, Data)),
      NewData = maps:update(creatures, NewCreatures, Data),
      {keep_state, NewData, {reply, From, ok}}
  end;

% Handle Enter on other states
handle_event({call, From}, {enter, _}, Data) ->
  {keep_state, Data, {reply, From, {error, "Can't enter in this state"}}};

% Shutdown can be called in any state
handle_event({call, From}, {shutdown, NextPlane}, Data) ->
  NextPlane ! {shutting_down, From, maps:to_list(maps:get(creatures, Data))},
  {next_state, shutting_down, Data, {next_event, internal, {From, NextPlane}}};

handle_event({call, From}, {trigger, _Trigger}, Data) ->
  {keep_state, Data, {reply, From, {error, "Can't set a trigger in this state"}}};

handle_event({call, From}, {connect, _Action, _To}, Data) ->
  {keep_state, Data, {reply, From, {error, "Can't connect in this state"}}};

% ignore all other unhandled events
handle_event(_EventType, _EventContent, Data) ->
  {keep_state, Data}.

under_configuration({call, From}, {connect, Action, To}, Data) ->
  case is_process_alive(To) of
    true -> case maps:is_key(Action, maps:get(connections, Data)) of
              false -> Connections = maps:put(Action, To, maps:get(connections, Data)),
                NewData = maps:update(connections, Connections, Data),
                {keep_state, NewData, {reply, From, ok}};
              true -> {keep_state, Data, {reply, From, {error, "Action already exists"}}}
            end;
    false -> {keep_state, Data, {reply, From, {error, "Process not alive anymore"}}}
  end;

under_configuration({call, From}, activate, Data) ->
  {next_state, under_activation, Data, {next_event, internal, From}};


under_configuration({call, From}, {trigger, Trigger}, Data) ->
  NewData = maps:update(trigger, Trigger, Data),
  {keep_state, NewData, {reply, From, ok}};

%% General Event Handling for state under_configuration
under_configuration(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

under_activation(internal, From, Data) ->
  Result = broadcast_connection(maps:to_list(maps:get(connections, Data)), From, active),
  case Result of
    impossible -> {next_state, under_configuration, Data, {reply, From, Result}};
    active -> {next_state, active, Data, {reply, From, Result}}
  end;

under_activation({call, From}, activate, Data) ->
  {keep_state, Data, {reply, From, under_activation}};

%% General Event Handling for state under_activation
under_activation(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

active({call, From}, {enter, {Ref, Stats}}, Data) ->
  case maps:is_key(Ref, maps:get(creatures, Data)) of
    true -> {keep_state, Data, {reply, From, {error, "Creture is already in this District"}}};
    false -> Creatures = maps:get(creatures, Data),
      case maps:get(trigger, Data) of
        none -> Creature1 = none, Creatures1 = none;
        Trigger -> case run_trigger(Trigger, entering, {Ref, Stats}, Creatures) of
                     {error, _} -> Creature1 = none, Creatures1 = none;
                     {Creature1, Creatures1} -> {Creature1, Creatures1}
                   end
      end,
      case {Creature1, Creatures1} of
        {none, none} -> NewCreatures = maps:put(Ref, Stats, maps:get(creatures, Data)),
          NewData = maps:update(creatures, NewCreatures, Data);
        {{Ref1, Stats1}, NewCreatures1} -> NewCreatures = maps:put(Ref1, Stats1, maps:from_list(NewCreatures1)),
          NewData = maps:update(creatures, NewCreatures, Data)
      end,
      {keep_state, NewData, {reply, From, ok}}
  end;

active({call, From}, {take_action, CRef, Action}, Data) ->
  case maps:is_key(Action, maps:get(connections, Data)) of
    true ->
      case maps:is_key(CRef, maps:get(creatures, Data)) of
        false -> {keep_state, Data, {reply, From, {error, "Creature doesn't exist in this district"}}};
        true -> case maps:get(trigger, Data) of
                  none -> Creature1 = none, Creatures1 = none;
                  Trigger ->
                    RemoveCreature = maps:remove(CRef, maps:get(creatures, Data)),
                    RemovedData = maps:update(creatures, RemoveCreature, Data),
                    case run_trigger(Trigger, leaving, {CRef, maps:get(CRef, maps:get(creatures, Data))},
                      maps:get(creatures, RemovedData)) of
                      {error, _} -> Creature1 = none, Creatures1 = none;
                      {Creature1, Creatures1} -> {Creature1, Creatures1}
                    end
                end,
          case {Creature1, Creatures1} of
            {none, none} -> NewDataCreatures = Data;
            {{Ref, Stats}, _} -> NewCreatures = maps:put(Ref, Stats, maps:get(creatures, Data)),
              NewDataCreatures = maps:update(creatures, NewCreatures, Data)
          end,
          {NewData, To} = creature_leave(CRef, Action, From, NewDataCreatures),
          case NewData of
            error -> {keep_state, Data, {reply, From, {error, To}}};
            _ -> {keep_state, NewData, {reply, From, {ok, To}}}
          end
      end;
    false -> {keep_state, Data, {reply, From, {error, "Action doesn't exist"}}}
  end;

active({call, From}, activate, Data) ->
  {keep_state, Data, {reply, From, active}};

%% Handle Calls to active
active(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

shutting_down(internal, {From, NextPlane}, Data) ->
  Result = broadcast_shutdown(maps:to_list(maps:get(connections, Data)), From, NextPlane),
  {stop_and_reply, normal, {reply, From, Result}};

shutting_down({call, From}, activate, Data) ->
  {keep_state, Data, {reply, From, impossible}};

shutting_down({call, From}, options, Data) ->
  {keep_state, Data, {reply, From, none}};

shutting_down({call, From}, shutdown, Data) ->
  {keep_state, Data, {reply, From, ok}};

%% Handle Calls to shutting_down
shutting_down(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

% initial State under_configuration
init(Desc) ->
  %% Set the initial state + data
  State = under_configuration, Data = #{description => Desc, connections => #{}, creatures => #{}, trigger => none},
  {ok, State, Data}.

callback_mode() -> state_functions.

%% Synchronous Call which should wait until each response
broadcast_shutdown([], _, _NextPlane) -> ok;
broadcast_shutdown([{_Action, To} | Actions], {Pid, Ref}, NextPlane) ->
  case is_process_alive(To) of
    true ->
      case term_to_binary(To) == term_to_binary(Pid) of
        true -> void;
        false -> case term_to_binary(To) == term_to_binary(self()) of
                   true -> void;
                   false -> gen_statem:call(To, {shutdown, NextPlane})
                 end
      end;
    false -> void
  end,
  broadcast_shutdown(Actions, {Pid, Ref}, NextPlane).

%% Synchronous Call which should wait until each response
broadcast_connection([], _, Result) -> Result;
broadcast_connection([{_Action, To} | Actions], {Pid, Ref}, _) ->
  case is_process_alive(To) of
    false -> Result1 = impossible;
    true -> Result1 = active,
      case term_to_binary(To) == term_to_binary(Pid) of
        false -> case term_to_binary(To) == term_to_binary(self()) of
                   true -> void;
                   false -> gen_statem:call(To, activate)
                 end;
        true -> void
      end
  end,
  broadcast_connection(Actions, {Pid, Ref}, Result1).

creature_leave(CRef, Action, {_Pid, _}, Data) ->
  To = maps:get(Action, maps:get(connections, Data)),
  Stats = maps:get(CRef, maps:get(creatures, Data)),
  case is_process_alive(To) of
    true -> case term_to_binary(self()) == term_to_binary(To) of
              true -> {Data, To};
              false -> case gen_statem:call(To, {run_action, CRef, Stats}) of
                         ok -> NewCreatures = maps:remove(CRef, maps:get(creatures, Data)),
                           NewData = maps:update(creatures, NewCreatures, Data),
                           {NewData, To};
                         {error, Reason} -> {error, Reason}
                       end
            end;
      false -> {error, "District is shutdown"}
end .

run_trigger(Trigger, Event, Creature, Creatures) ->
  Self = self(),
  spawn(fun() -> Self ! {self(), Trigger(Event, Creature, maps:to_list(Creatures))} end),
  receive
    {_Pid, {Creature1, Creatures1}} -> {Creature1, Creatures1}
  after
    2000 -> {error, "didnt't run function"}
  end.

