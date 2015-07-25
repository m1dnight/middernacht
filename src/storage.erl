-module(storage).
-compile(export_all).

-include_lib("records.hrl").


%%TODO
% - Use record for loop state.
% - Allow a filter for a get_peers request to filter out request ip.

%%===============================================================================
%% INTERFACE
%%===============================================================================

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

%% Starts an actor responsible for storage and registers its name as 'store'.
init() ->
    loop(dict:new()).


%%-------------------------------------------------------------------------------
%% MESSAGE INTERFACE
%%-------------------------------------------------------------------------------

remove(Key) ->
    ?MODULE ! {remove, Key}.

insert(Key, Value) ->
    ?MODULE ! {insert, Key, Value}.

get_peers() ->
    ?MODULE ! {get_peers, self()},
    receive 
        {ok, Peers} -> Peers
    end.

print_status() ->
    ?MODULE ! print_status.


%%===============================================================================
%% Innards
%%===============================================================================

%%-------------------------------------------------------------------------------
%% SERVER LOOP
%%-------------------------------------------------------------------------------
loop(Peers) ->
    receive
        {insert, Identifier, PeerRecord} ->
            NewDict = dict:update(Identifier, fun(_) -> PeerRecord end, 
                                  PeerRecord, 
                                  Peers),
            loop(NewDict);

        {remove, Identifier} ->
            loop(dict:erase(Identifier, Peers));

        %% Returns all the peers in a list.
        %% Each peer is formatted in such a way that it can be easily bencoded.
        %% I.e., {dict, <dict with data of peer>}.
        {get_peers, Sender} ->
            PeerList = dict:fold(fun(_Id, PeerRecord, {Seeders,Leechers}) ->
                                         Bencd = format_peer_entry(PeerRecord), 
                                         if PeerRecord#peer.isseeder ->
                                                 {[Bencd | Seeders], Leechers};
                                            true  ->
                                                 {Seeders, [Bencd | Leechers]}
                                         end
                                 end,
                                 {[],[]},
                                 Peers),
            Sender ! {ok, PeerList};
        
        %% Prints the current peer database on screen. Debugging purposes.
        print_status ->
            dict:map(fun(_Id, P) ->
                             io:fwrite("~-10.s:~s~n",    ["Peer ID:", P#peer.id]),
                             io:fwrite("~-10.s:~s:~w~n", ["Peer net:", P#peer.address, P#peer.port]),
                             io:fwrite("~-10.s:~s~n", ["Seeder?:", P#peer.isseeder])
                     end, 
                     Peers);
        
        shutdown ->
            exit(shutdown)
    end,
    loop(Peers).


%%-------------------------------------------------------------------------------
%% HELPERS
%%-------------------------------------------------------------------------------

%% Takes a Peer record and returns a dictionary that can be encoded by the
%% bencoder.
format_peer_entry(Peer) ->
    PeerId  = Peer#peer.id,
    Address = Peer#peer.address,
    Port    = Peer#peer.port,
    {dict, dict:from_list([{"ip", Address}, {"port", Port}, {"peer id", PeerId}])}.
