%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy w modelu hybrydowym.

-module(hybrid_island).
-export([start/0, close/1, sendAgent/2]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia glowna
%% petle procesu.
-spec start() -> ok.
start() ->
    misc_util:seedRandom(),
    Environment = config:agent_env(),
    Agents = Environment:initial_population(),
    timer:send_interval(config:writeInterval(), write),
    loop(Agents, misc_util:createNewCounter(), Environment:stats()).

-spec close(pid()) -> {finish, pid()}.
close(Pid) ->
    Pid ! {finish, self()}.

%% @doc Funkcja za pomoca ktorej mozna przesylac wyspie imigrantow.
%% Komunikacja asynchroniczna.
-spec sendAgent(pid(), agent()) -> {agent, pid(), agent()}.
sendAgent(Pid, Agent) ->
    Pid ! {agent, self(), Agent}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc Glowna petla procesu. Kazda iteracja powoduje wytworzenie kolejnej generacji.
-spec loop([agent()], counter(), [tuple()]) -> ok.
loop(Agents, InteractionCounter, Funstats) ->
    Environment = config:agent_env(),
    receive
        write ->
            [logger:log_countstat(self(), Interaction, Val) || {Interaction, Val} <- dict:to_list(InteractionCounter)],
            [logger:log_funstat(self(), StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- Funstats],
            loop(Agents, misc_util:createNewCounter(), Funstats);
        {agent, _Pid, A} ->
            loop([A|Agents], InteractionCounter, Funstats);
        {finish, _Pid} ->
          io:format("Stats: ~p~n", [getstats(Agents)]),
            ok
    after 0 ->
            Groups = misc_util:groupBy([{Environment:behaviour_function(A), A} || A <- Agents ]),
            NewGroups = [misc_util:meeting_proxy(G, hybrid) || G <- Groups],
      NewAgents = lists:flatten(NewGroups),
            %NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),

            NewFunstats = misc_util:count_funstats(NewAgents, Funstats),
            NewCounter = misc_util:add_interactions_to_counter(Groups, InteractionCounter),

            loop(NewAgents, NewCounter, NewFunstats)
    end.

getstats(Island) ->
  lists:foldl(fun(Agent, Res) ->
    {{min, {MinWholeClappIter, MinChangeNumber}}, {max, {MaxWholeClappIter, MaxChangeNumber}}, {iter, _}, {clapcounter, ClapCounter}} = Res,
    {_, {State, _}, {WholeIter, WholeClappIter, ChangeNumber}} = Agent,
    if
      WholeClappIter>MaxWholeClappIter ->
        NewMaxWholeClappIter=WholeClappIter;
      true ->
        NewMaxWholeClappIter=MaxWholeClappIter
    end,
    if
      ChangeNumber>MaxChangeNumber ->
        NewMaxChangeNumber=ChangeNumber;
      true ->
        NewMaxChangeNumber=MaxChangeNumber
    end,
    if
      WholeClappIter<MinWholeClappIter ->
        NewMinWholeClappIter=WholeClappIter;
      true ->
        NewMinWholeClappIter=MinWholeClappIter
    end,
    if
      ChangeNumber<MinChangeNumber ->
        NewMinChangeNumber=ChangeNumber;
      true ->
        NewMinChangeNumber=MinChangeNumber
    end,
    if
      State==1 ->
        NewClapCounter=ClapCounter+1;
      true ->
        NewClapCounter=ClapCounter
    end,
    {{min, {NewMinWholeClappIter, NewMinChangeNumber}}, {max, {NewMaxWholeClappIter, NewMaxChangeNumber}}, {iter, WholeIter}, {clapcounter, NewClapCounter}}
  end, {{min, {999999, 999999}}, {max, {0, 0}}, {iter, 0}, {clapcounter, 0}}, Island).