-module (audience_config).

-compile(export_all).

%% ====================================================================
%% AUDIENCE PARAMETERS
%% ====================================================================

%% @doc Rozmiar problemu (wymiary siatki)
sizeX() -> 10.
sizeY() -> 10.

%% @doc Jaka część sąsiedztwa musi być w stanie 1, aby przyjąć ten stan; w przeciwnym wypadku stan bez zmian (zakres od 0 do 1)
changeToClappingStateRate() -> 0.5.

%% @doc Prawdopodobienstwo, ze agent zdecyduje się zmienic stan na 1 (pod warunkiem spełnienia changeToClappingStateRate)
%% Zakres od 0 do 100 (procentowy)
startClappingProb() -> 80.

%% @doc Parametr okreslajacy po ilu iteracjach agent moze zaczac sie starac o zmiane stanu z 1 na 0
stopClappingIter() -> 5.

%% @doc Prawdopodobienstwo, ze agent zdecyduje się zmienic stan na 0 (pod warunkiem spełnienia stopClappingIter)
%% Zakres od 0 do 100 (procentowy)
stopClappingProb() -> 80.

%% @doc Parametr okreslajacy jak duza czesc agentow w nowej populacji ma przyjmować stan 1 na wstępie
%% Zakres od 0 do 100 (procentowy)
initClappingProb() -> 35.
