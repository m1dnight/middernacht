-module(storage).
-compile(export_all).

-include_lib("../include/records.hrl").


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 1995/01/05 13:04 13:04:07 ').

%%===============================================================================
%% API
%%===============================================================================

%% Starts an actor responsible for storage and registers its name as 'store'.
init() ->
    Storage = spawn(?MODULE, memory_store, [dict:new()]),
    register(store, Storage).


%%-------------------------------------------------------------------------------
%% Messages
%% ------------------------------------------------------------------------------
remove(StoreId, Key) ->
    StoreId ! {remove, Key}.

insert(StoreId, Key, Value) ->
    StoreId ! {insert, Key, Value}.

get_peers(StoreId) ->
    StoreId ! {get_peers, self()},
    receive 
        {ok, Peers} -> Peers
    end.

print_status(StoreId) ->
    StoreId ! {print_status}.
%%-------------------------------------------------------------------------------
%% Innards
%% ------------------------------------------------------------------------------




%% Takes a Peer record and returns a dictionary that can be encoded by the
%% included bencoder.
format_peer_entry(Peer) ->
    PeerId  = Peer#peer.id,
    Address = Peer#peer.address,
    Port    = Peer#peer.port,
    {dict, dict:from_list([{"ip", Address}, {"port", Port}, {"peer id", PeerId}])}.


memory_store(Peers) ->
    receive
        %% Insert a new peer into the database.
        {insert, Identifier, PeerRecord} ->
            NewDict = dict:update(Identifier, fun(_) -> 
                                                      PeerRecord 
                                              end, 
                                  PeerRecord, 
                                  Peers),
            memory_store(NewDict);

        %% Removes a peer from storage.
        {remove, Identifier} ->
            NewDict = dict:erase(Identifier, Peers),
            memory_store(NewDict);

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

        {print_status} ->
            dict:map(fun(_Id, P) ->
                             io:fwrite("~-10.s:~s~n",    ["Peer ID:", P#peer.id]),
                             io:fwrite("~-10.s:~s:~w~n", ["Peer net:", P#peer.address, P#peer.port]),
                             io:fwrite("~-10.s:~s~n", ["Seeder?:", P#peer.isseeder])
                     end, 
                     Peers)      
    end,
    memory_store(Peers).



