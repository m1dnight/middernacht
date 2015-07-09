-module(tracker_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/records.hrl").


create_peer(PeerId) ->
    Port = 1234,
    Hash = [22,241,173,15,66,217,218,72,33,1,126,106,46,204,185,219,244,36,36,230],
    Key = "253c40bd",
    Identifier = PeerId++Hash,
    Peer = #peer{address="192.168.1.1", id=PeerId, port=Port, key=Key, isseeder=false},
    {Peer, Identifier}.


bencode_peer_test() ->
    {Peer, _Identifier} = create_peer("-qB3180-QJcFPXudj-kF"),
    Expected = <<"d2:ip11:192.168.1.17:peer id20:-qB3180-QJcFPXudj-kF4:porti1234ee">>,
    Formatted = main:format_peer(Peer),
    Expected = bencode:encode(Formatted).

peerdata_test() ->
    {Peer, Identifier} = create_peer("-qB3180-QJcFPXudj-kF"),
    {Peer2, Identifier2} = create_peer("-qB3180-QJcFPXudj-kG"),
    %% Start new peerdata
    PeerData = spawn(main, peerdata, [dict:new()]),
    %% Insert a single peer
    PeerData ! {insert, Identifier, Peer},
    PeerData ! {insert, Identifier2, Peer2},
    %% Test fetch peers
    PeerData ! {get_peers, self()},
    receive
        {ok, Data} ->
            io:fwrite("Data: ~p", [Data]),
            lists:map(fun({dict, Entry}) -> io:fwrite("~n~w~n", [bencode:encode({dict, Entry})]), ok end, Data),
            bencode:encode({list, Data})
            
    end.


%%  c('../bencoding/bencode.erl'),c('../tracker/main.erl'), c(tracker_test).
%%  tracker_test:peerdata_test().



