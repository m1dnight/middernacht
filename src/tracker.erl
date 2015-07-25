-module(tracker).
-compile(export_all).

-include_lib("records.hrl").

%% State for tracker.erl
-record(state, {storageservice,
                socketservice,
                port}).


%%===============================================================================
%% INTERFACE
%%===============================================================================

start(Port) ->
    register(?MODULE, Pid=spawn(?MODULE, init, [Port])),
    Pid.

start_link(Port) ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [Port])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

init(Port) ->
    %% Start a storage service.
    StorageService = storage:start_link(),
    %% Start a socket service.
    SocketService = listener:start_link(Port),
    loop(#state{storageservice=StorageService,
                socketservice=SocketService,
                port=Port}).

%%===============================================================================
%% INNARDS
%%===============================================================================

%%-------------------------------------------------------------------------------
%% SERVER LOOP
%%-------------------------------------------------------------------------------

loop(State=#state{}) ->
    receive 
        shutdown ->
            exit(shutdown);
        status ->
            io:fwrite("Status of tracker: ~n~p~n", [State])
    end.
