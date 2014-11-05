%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. cze 2014 08:34
%%%-------------------------------------------------------------------
-module(audience).
-author("Administrator").
-behaviour(agent_env).
-include ("mas.hrl").
%% API
-export([starts/1, start/2, start/3, initial_population/0, behaviour_function/1, behaviours/0, meeting_function/1, stats/0]).

-spec start(model(),pos_integer()) -> ok.
start(Model, Time) ->
  mas:start(?MODULE,Model,Time,[]).

-spec starts([list()]) -> ok.
starts(Args) ->
  [Model, Time] = Args,
  mas:start(?MODULE,erlang:list_to_atom(Model),erlang:list_to_integer(Time),[]).

-spec start(model(),pos_integer(),[tuple()]) -> ok.
start(Model, Time, Options) ->
  mas:start(?MODULE,Model,Time,Options).


-spec initial_population() -> [agent()].
initial_population() ->
  aud_agent:generatePopulation({audience_config:sizeX(), audience_config:sizeY()}).

-spec behaviour_function(agent()) -> agent_behaviour().
behaviour_function(Agent) ->
  {{_, Y}, {_, _}, {WholeIter, _, _}} = Agent,
  if
    WholeIter rem 2 == 0 ->
      act;
    true ->
      if
        Y == 1 ->
          migration;
        true ->
          wait
      end
  end.

-spec behaviours() -> [agent_behaviour()].
behaviours() ->
  [act, wait, migration].

-spec meeting_function({agent_behaviour(), [agent()]}) -> [agent()].
meeting_function({act, Agents}) ->
  AgentsWithRate=aud_agent:getAgentsWithRate(Agents),
  NewAgents=lists:map(fun aud_agent:changeStateTo1/1, AgentsWithRate),
  NewAgents2=lists:map(fun aud_agent:changeStateTo0/1, NewAgents),
  %io:format("~p After act: ~p~n", [self(), NewAgents2]),
  NewAgents2;


meeting_function({wait, Agents}) ->
  Agents;

meeting_function({migration, Agents}) ->
  Agents;

meeting_function({_, _}) ->
  erlang:error(unexpected_behaviour).

-spec stats() -> [funstat()].
stats() ->
  Map_fun = fun(Agent) ->
%%     io:format("~p~n", [Agent]),
    {_Key, {_State, _CurrClapIter}, {WholeIter, WholeClappIter, ChangeNumber}} = Agent,
    {WholeIter, WholeClappIter, ChangeNumber}

  end,
  Reduce_fun = fun(F1, F2) ->
    {_,MaxWholeClappIter, MaxChangeNumber}=F2,
    {F1WholeIter, F1WholeClappIter, F1ChangeNumber}=F1,
    if
      F1WholeClappIter>MaxWholeClappIter ->
        NewMaxWholeClappIter=F1WholeClappIter;
      true ->
        NewMaxWholeClappIter=MaxWholeClappIter
    end,
    if
      F1ChangeNumber>MaxChangeNumber ->
        NewMaxChangeNumber=F1ChangeNumber;
      true ->
        NewMaxChangeNumber=MaxChangeNumber
    end,
    {F1WholeIter, NewMaxWholeClappIter, NewMaxChangeNumber}
    %lists:max([F1|F2])
  end,
  [{audience_stats, Map_fun, Reduce_fun, {0, 0, 0}}].