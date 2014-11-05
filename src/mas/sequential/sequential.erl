%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(sequential).
-export([start/4]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time, Islands, Topology, Path) ->
    %%     io:format("{Model=sequential,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    topology:start_link(self(), Islands, Topology),
    Environment = config:agent_env(),
    InitIslands = [Environment:initial_population() || _ <- lists:seq(1, Islands)],
    logger:start_link(lists:seq(1, Islands), Path),
    timer:send_after(Time, theEnd),
    %{ok, TRef} = timer:send_interval(config:writeInterval(), write),
    {_Time,_Result} = timer:tc(fun loop/3, [
                                            InitIslands,
                                            [misc_util:createNewCounter() || _ <- lists:seq(1, Islands)],
                                            [Environment:stats() || _ <- lists:seq(1, Islands)]]),
    %timer:cancel(TRef),
    topology:close(),
    logger:close().

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
-spec loop([island()], [counter()], [funstat()]) -> float().
loop(Islands, Counters, Funstats) ->
    Environment = config:agent_env(),
    receive
        write ->
            [log_island(Nr, C, F) || {Nr, C, F} <- lists:zip3(lists:seq(1, length(Islands)), Counters, Funstats)],
            loop(Islands,
                [misc_util:createNewCounter() || _ <- lists:seq(1, length(Islands))],
                Funstats);
        theEnd ->
          %%[]
%%           [io:format("~p~n", [misc_util:result(I)]) || I <- Islands]
           %Res=lists:max([misc_util:result(I) || I <- Islands]),
          %io:format("End population ~p~n", [Islands]),
          %io:format("Res: ~p~n", [Res]),
          io:format("Stats: ~p~n", [getstats(lists:flatten(Islands))])
    after 0 ->
        Groups = [misc_util:groupBy([{Environment:behaviour_function(Agent),Agent} || Agent <- I]) || I <- Islands],
      %io:format("Groups: ~p~n", [Groups]),
         Emigrants = [seq_migrate(lists:keyfind(migration, 1, Island), Nr) || {Island, Nr} <- lists:zip(Groups, lists:seq(1, length(Groups)))],
       %io:format("Emigrants: ~p~n", [Emigrants]),
      %[[io:format("Activity: ~p~n", [Activity]) || Activity <- I] || I <- Groups],
        NewGroups = [[misc_util:meeting_proxy(Activity, sequential) || Activity <- I] || I <- Groups],
      %io:format("NewGroups: ~p~n", [NewGroups]),
        WithEmigrants = append(lists:flatten(Emigrants), NewGroups),
      NewIslands = [aud_agent:incrementIterations(lists:flatten(I)) || I <- WithEmigrants],
      %io:format("NewIslands: ~p~n", [NewIslands]),
        %NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- WithEmigrants],
      %NewCounters=Counters,
      %NewFunstats=Funstats,
        NewCounters = [misc_util:add_interactions_to_counter(G, C) || {G, C} <- lists:zip(Groups, Counters)],
      %io:format("NewCounters ~p~n", [NewCounters]),
        NewFunstats = [misc_util:count_funstats(I, F) || {I, F} <- lists:zip(NewIslands, Funstats)],
      %io:format("NewFunstats ~p~n", [NewFunstats]),

        loop(NewIslands, NewCounters, NewFunstats)
    end.

%% -spec log(pos_integer(), [agent()], counter(), [funstat()]) -> [ok].
%% log(Nr, _Island, Counter, []) ->
%%     [logger:log_countstat(Nr, Stat, Val) || {Stat, Val} <- dict:to_list(Counter)];
%%
%% log(Nr, Island, Counter, [{StatName, MapFun, ReduceFun, InitVal}|Stats]) ->
%%     StatVal = lists:foldl(ReduceFun,
%%                           InitVal,
%%                           [MapFun(Agent) || Agent <- Island]),
%%     logger:log_funstat(Nr, StatName, StatVal),
%%     log(Nr, Island, Counter, Stats).

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

-spec log_island(pos_integer(), counter(), [funstat()]) -> [ok].
log_island(Key, Counter, Funstats) ->
    [logger:log_countstat(Key, Interaction, Val) || {Interaction, Val} <- dict:to_list(Counter)],
    [logger:log_funstat(Key, StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- Funstats].


-spec seq_migrate(false | {migration,[agent()]}, pos_integer()) -> [{migration,[agent()]}].
seq_migrate(false,_) ->
    [];

seq_migrate({migration,Agents},From) ->
    Destinations = [{topology:getDestination(From),Agent} || Agent <- Agents],
    misc_util:groupBy(Destinations).


-spec append({pos_integer(),[agent()]}, [list(agent())]) -> [list(agent())].
append([],Islands) ->
    Islands;

append([{Destination,Immigrants}|T],Islands) ->
    NewIslands = misc_util:mapIndex(Immigrants,Destination,Islands,fun lists:append/2),
    append(T,NewIslands).