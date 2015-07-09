-module(storage).
-compile(export_all).

-include_lib("../include/records.hrl").


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 1995/01/05 13:04 13:04:07 ').

peerdata(Peers) ->
    receive
        %% Insert a new peer into the database.
        {insert, Identifier, PeerRecord} ->
            NewDict = dict:update(Identifier, fun(_) -> 
                                                      PeerRecord 
                                              end, 
                                  PeerRecord, 
                                  Peers),
            peerdata(NewDict);
        %% Removes a peer from storage.
        {remove, Identifier} ->
            NewDict = dict:erase(Identifier, Peers),
            peerdata(NewDict);
        %% Returns all the peers in a list.
        %% Each peer is formatted in such a way that it can be easily bencoded.
        %% I.e., {dict, <dict with data of peer>}.
        {get_peers, Sender} ->
            PeerList = dict:fold(fun(_Id, PeerRecord, {Seeders,Leechers}) ->
                                         Bencd = format_peer(PeerRecord), 
                                         if PeerRecord#peer.isseeder ->
                                                 {[Bencd | Seeders], Leechers};
                                            true  ->
                                                 {Seeders, [Bencd | Leechers]}
                                         end
                                 end,
                                 {[],[]},
                                 Peers),
            Sender ! {ok, PeerList}
    end,
    peerdata(Peers).
