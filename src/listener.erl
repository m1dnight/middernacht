-module(listener).
-compile(export_all).

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
    %%TODO Case this bitch
    {ok, ListenSock} = gen_tcp:listen(Port, [list,
                                             {active, false},
                                             {packet,http}, 
                                             {reuseaddr,true}]),
    loop(ListenSock).


%%===============================================================================
%% INNARDS
%%===============================================================================

%%-------------------------------------------------------------------------------
%% SERVER LOOP
%%-------------------------------------------------------------------------------

loop(ListenSock) ->
    Result = gen_tcp:accept(ListenSock),
    case Result of
         {ok, Sock} ->
            handler:start(Sock);
        _  ->
            error_logger:error_report(io_lib:format("Error accepting on socket! ~p~n", [Result]))
    end,
    loop(ListenSock).
