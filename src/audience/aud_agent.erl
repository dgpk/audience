%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. cze 2014 09:02
%%%-------------------------------------------------------------------
-module(aud_agent).
-author("Administrator").
-include("audience.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([generatePopulation/1, changeStateTo0/1, changeStateTo1/1, incrementIterations/1, getAgentsWithRate/1]).

%% @doc Funkcja generujaca stan początkowy agenta
-spec generateState() -> agent_state().
generateState() ->
  ProbNum = round(audience_config:initClappingProb()),
  RndNum = random:uniform(100),
  %% Jeśli wylosujemy daną liczbę (prawd zadane w config'u), to stan ustawiamy na 1
  if
    RndNum =< ProbNum ->
      {1, 0};
    true ->
      {0, 0}
  end.

%% @doc Funkcja generujaca losowa populacje.
-spec generatePopulation({pos_integer(), pos_integer()}) -> [agent()].
generatePopulation(ProblemSize) ->
  Population = generateGridOfAgents(ProblemSize),
  %io:format("~p Initial population: ~p~n", [self(), Population]),
  Population.
  %[generateGridOfAgents(ProblemSize) || _ <- lists:seq(1, config:populationSize())]. %% Powtarza problem razy populationSize

%% @doc Funkcja zmiany stanu na clapping
-spec changeStateTo1(agent()) -> agent().
changeStateTo1(AgentWithRate) ->
  {{Key, {State, CurrClapIter}, {WholeIter, WholeClappIter, ChangeNumber}}, StateRate} = AgentWithRate,
  if
    State == 1 ->
      {Key, {State, CurrClapIter+1}, {WholeIter, WholeClappIter+1, ChangeNumber}};
    true ->
      %% Jeśli odpowiednia czesc sasiedztwa jest w stanie 1, to zmieniamy stan na 1
      ChangeRate = audience_config:changeToClappingStateRate(),
      ProbNum = round(audience_config:startClappingProb()),
      RndNum = random:uniform(100),
      if
        StateRate >= ChangeRate, RndNum =< ProbNum ->
          %Tylko tu jest zmiana stanu
          %io:format("ZMIANA ~p~n", [Key]),
          {Key, {1, 1}, {WholeIter, WholeClappIter+1, ChangeNumber+1}};
        true ->
          {Key, {State, CurrClapIter}, {WholeIter, WholeClappIter, ChangeNumber}}
      end
  end.

%% @doc Funkcja zmiany stanu na look_around
-spec changeStateTo0(agent()) -> agent().
changeStateTo0(Agent) ->
  {Key, {State, CurrClapIter}, {WholeIter, WholeClappIter, ChangeNumber}} = Agent,
  if
    State == 0 ->
      Agent;
    true ->
      %% Czy osiągnął max iter
      MaxIter = audience_config:stopClappingIter(),
      %% i losujemy zgodnie z prawd.
      ProbNum = round(audience_config:stopClappingProb()),
      RndNum = random:uniform(100),
      if
        CurrClapIter == MaxIter, RndNum =< ProbNum ->
          {Key, {0, 0}, {WholeIter, WholeClappIter, ChangeNumber+1}};
        true ->
          {Key, {State, CurrClapIter}, {WholeIter, WholeClappIter, ChangeNumber}}
      end
  end.

%% @doc Funkcja otoczenia
-spec getAgentsWithRate([agent()]) -> [{agent(), pos_integer()}].
getAgentsWithRate(Agents) ->
  %% Pobieramy wszystkich agentów sąsiadujących, a dokładnie ich stany
  %% a następnie je sumujemy i dzielimy przez ilosc sąsiadów
  %% Zwracamy parę {Agent, StosunekStanówSąsiednich}
  lists:map(fun(Agent) ->
    {{PosX, PosY}, _, _} = Agent,
    List = [State || {{X, Y}, {State, _}, _} <- Agents, ({X, Y}=:={PosX, PosY-1}) or ({X, Y}=:={PosX, PosY+1})
      or ({X, Y}=:={PosX+1, PosY-1}) or ({X, Y}=:={PosX+1, PosY}) or ({X, Y}=:={PosX+1, PosY+1})],
    S = lists:foldl(fun(State, Sum) -> State + Sum end, 0, List),
    {Agent, S/lists:flatlength(List)}
  end, Agents).

%% @doc Funkcje generujące siatkę
-spec generateGridOfAgents({pos_integer(), pos_integer()}) -> [agent()].
generateGridOfAgents({X, Y}) when X>0; Y>0 ->
  generateGridOfAgents({X, Y}, Y, []).

%% @doc Funkcje generujące siatkę
-spec generateGridOfAgents({pos_integer(), pos_integer()}, pos_integer(), [agent()]) -> [agent()].
generateGridOfAgents({1, 0}, _Size_Y, Population) ->
  Population;
generateGridOfAgents({X, 0}, Size_Y, Population) ->
  generateGridOfAgents({X-1, Size_Y}, Size_Y, Population);
generateGridOfAgents({X, Y}, Size_Y, Population) ->
  NewPopulation = [{{X, Y}, generateState(), {0, 0, 0}}|Population],
  generateGridOfAgents({X, Y-1}, Size_Y, NewPopulation).

%% @doc Funkcja inkrementująca informację o ilości iteracji dla każdego agenta
-spec incrementIterations([agent()]) -> [agent()].
incrementIterations(Agents) ->
  [{Key, FullState, {WholeIter+1, WholeClappIter, ChangeNumber}}
    || {Key, FullState, {WholeIter, WholeClappIter, ChangeNumber}} <- Agents].


%% -spec incrementIteration(agent()) -> agent().
%% incrementIteration(Agent) ->
%%   {Key, FullState, {WholeIter, WholeClappIter, ChangeNumber}}=Agent,
%%   {Key, FullState, {WholeIter+1, WholeClappIter, ChangeNumber}}.


%% Funkcja generujaca losowego agenta
%% Agent { {posX, posY}, state }
%% -spec generateAgent(audience:position()) -> audience:agent().
%% generateAgent(Position) ->
%%   {PosX, PosY} = Position,
%%   State = 0,
%%   {{PosX, PosY}, State}.

%% %% Funkcja otoczenia
%% checkState(Key) ->
%%   {PosX, PosY} = Key,
%%   %% Pobieramy otoczenie (boki + przód + przedni skos) -> max 5, min 1
%%   MS = ets:fun2ms(fun({{X, Y}, State}) when ({X, Y}=:={PosX, PosY-1}) or ({X, Y}=:={PosX, PosY+1}) or ({X, Y}=:={PosX+1, PosY-1}) or ({X, Y}=:={PosX+1, PosY}) or ({X, Y}=:={PosX+1, PosY+1}) -> {{X, Y}, State} end),
%%   List = ets:select(audience_config:etsName(), MS),
%%   %% Sumujemy stan i dzielimy przez liczbę sąsiadów -> otrzymujemy stosunek liczby klaskających do wszytskich
%%   S = lists:foldl(fun({{_, _}, State}, Sum) -> State + Sum end, 0, List),
%%   S/lists:flatlength(List).

%% %% Funkcja zmiany stanu na clapping
%% -spec changeStateTo1(agent()) -> agent().
%% changeStateTo1(AgentWithRate) ->
%%   {{Key, State}, StateRate} = AgentWithRate,
%%   if
%%     State == 1 ->
%%       {Key, State};
%%     true ->
%%       %% Jeśli odpowiednia czesc sasiedztwa jest w stanie 1, to zmieniamy stan na 1
%%       ChangeRate = audience_config:changeToClappingStateRate(),
%%       if
%%         StateRate >= ChangeRate ->
%%           {Key, 1};
%%         true ->
%%           {Key, State}
%%       end
%%   end.
%%
%% %% Funkcja zmiany stanu na look_around
%% -spec changeStateTo0(agent()) -> agent().
%% changeStateTo0(Agent) ->
%%   {Key, State} = Agent,
%%   if
%%     State == 0 ->
%%       Agent;
%%     true ->
%%       %A1,A2,A3} = now(),
%%       %random:seed(A1, A2, A3),
%%       MaxNum = round(1/audience_config:stopClappingRate()), %% Niestety, musi być int
%%       RndNum = random:uniform(MaxNum),
%%       %% Jeśli wylosujemy daną liczbę (prawd zadane w config'u), to zmieniamy stan (pozycja 2) na 0 dla danego klucza
%%       if
%%         RndNum == MaxNum ->
%%           {Key, 0};
%%         true ->
%%           {Key, State}
%%       end
%%   end.


%% %% Funkcja zmiany stanu na clapping
%% changeStateTo1(Agent) ->
%%   {Key, _} = Agent,
%%   %% Jeśli odpowiednia czesc sasiedztwa jest w stanie 1, to zmieniamy stan (pozycja 2) na 1 dla danego klucza
%%   StateRate = checkState(Key),
%%   ChangeRate = audience_config:changeToClappingStateRate(),
%%   if
%%     StateRate >= ChangeRate ->
%%       {Key, 1};
%%       %%ets:update_element(audience_config:etsName(), Key, {2, 1})
%%   true ->
%%     {Key, 0}
%%   end.
%%
%% %% Funkcja zmiany stanu na look_around
%% changeStateTo0(Agent) ->
%%   {A1,A2,A3} = now(),
%%   random:seed(A1, A2, A3),
%%   MaxNum = round(1/audience_config:stopClappingnRate()), %% Niestety, musi być int
%%   RndNum = random:uniform(MaxNum),
%%   %% Jeśli wylosujemy daną liczbę (prawd zadane w config'u), to zmieniamy stan (pozycja 2) na 0 dla danego klucza
%%   if
%%     RndNum == MaxNum ->
%%       {Key, _} = Agent,
%%       {Key, 0};
%%     true ->
%%       Agent
%%   end.

%% ETS ****************************

%% %% doc Funkcja otoczenia
%% checkState(Key) ->
%%   {PosX, PosY} = Key,
%%   %% Pobieramy otoczenie (boki + przód + przedni skos) -> max 5, min 1
%%   MS = ets:fun2ms(fun({{X, Y}, State}) when ({X, Y}=:={PosX, PosY-1}) or ({X, Y}=:={PosX, PosY+1}) or ({X, Y}=:={PosX+1, PosY-1}) or ({X, Y}=:={PosX+1, PosY}) or ({X, Y}=:={PosX+1, PosY+1}) -> {{X, Y}, State} end),
%%   List = ets:select(audience_config:etsName(), MS),
%%   %% Sumujemy stan i dzielimy przez liczbę sąsiadów -> otrzymujemy stosunek liczby klaskających do wszytskich
%%   S = lists:foldl(fun({{_, _}, State}, Sum) -> State + Sum end, 0, List),
%%   S/lists:flatlength(List).
%%
%% %% doc Funkcja zmiany stanu na clapping
%% changeStateTo1(Agent) ->
%%   {Key, _} = Agent,
%%   %% Jeśli odpowiednia czesc sasiedztwa jest w stanie 1, to zmieniamy stan (pozycja 2) na 1 dla danego klucza
%%   StateRate = checkState(Key),
%%   ChangeRate = audience_config:changeToClappingStateRate(),
%%   if
%%     StateRate >= ChangeRate ->
%%       ets:update_element(audience_config:etsName(), Key, {2, 1})
%%   end.
%%
%% %% doc Funkcja zmiany stanu na look_around
%% changeStateTo0(Agent) ->
%%   {A1,A2,A3} = now(),
%%   random:seed(A1, A2, A3),
%%   MaxNum = round(1/audience_config:stopClappingnRate()), %% Niestety, musi być int
%%   RndNum = random:uniform(MaxNum),
%%   %% Jeśli wylosujemy daną liczbę (prawd zadane w config'u), to zmieniamy stan (pozycja 2) na 0 dla danego klucza
%%   if
%%     RndNum == MaxNum ->
%%       {Key, _} = Agent,
%%       ets:update_element(audience_config:etsName(), Key, {2, 0})
%%   end.


%% -spec generateGridOfAgents({pos_integer(), pos_integer()}) -> ok.
%% generateGridOfAgents({X, Y}) when X>0; Y>0 ->
%%   ets:new(audience_config:etsName(), [ordered_set, named_table, {keypos, 1}]),
%%   generateGridOfAgents({X, Y}, Y).
%%
%% -spec generateGridOfAgents({pos_integer(), pos_integer()}, pos_integer()) -> ok.
%% generateGridOfAgents({1, 0}, _Size_Y) ->
%%   ok;
%% generateGridOfAgents({X, 0}, Size_Y) ->
%%   generateGridOfAgents({X-1, Size_Y}, Size_Y);
%% generateGridOfAgents({X, Y}, Size_Y) ->
%%   ets:insert(audience_config:etsName(), {{X, Y}, generateState()}),
%%   generateGridOfAgents({X, Y-1}, Size_Y).

%% ****************************


%% -spec generateGrid({pos_integer(), pos_integer()}) -> [audience:agent()].
%% generateGrid(ProblemSize) ->
%%   {SizeX, SizeY} = ProblemSize,
%%   lists:concat(
%%   lists:map(
%%     fun(X) ->
%%       lists:map(
%%         fun(Y) ->
%%           generateAgent({X, Y})
%%         end,
%%         lists:seq(1,SizeY))
%%     end,
%%     lists:seq(1,SizeX))).

%% -spec generateGridOfAgents({pos_integer(), pos_integer()}) -> [audience:agent()].
%% generateGridOfAgents({X, Y}) when X>0; Y>0 ->
%%   Dict = orddict:new(),
%%   generateGridOfAgents({X, Y}, Y, Dict).
%%
%% -spec generateGridOfAgents({pos_integer(), pos_integer()}, pos_integer(), orddict()) -> [audience:agent()].
%% generateGridOfAgents({1, 0}, _Size_Y, Dict) ->
%%   Dict;
%% generateGridOfAgents({X, 0}, Size_Y, Dict) ->
%%   generateGridOfAgents({X-1, Size_Y}, Size_Y, Dict);
%% generateGridOfAgents({X, Y}, Size_Y, Dict) ->
%%   %%orddict:store({X, Y}, "("++X++","++Y++")", Dict),
%%   %%io:format("(~p,~p)~n", [X, Y]),
%%   generateGridOfAgents({X, Y-1}, Size_Y, orddict:store({X, Y},
%%     generateState(),
%%     Dict)).