-module(main).
-compile(export_all).

-include_lib("records.hrl").


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 1995/01/05 13:04 13:04:07 ').

%%%%%%%%%%
%% TODO %%
%%%%%%%%%%
%% - Handle compact responses.

%%--------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------

start(Port) ->
    PeerData = spawn(?MODULE, peerdata, [dict:new()]),
    {ok, ListenSock} = gen_tcp:listen(Port, [list,
                                             {active, false},
                                             {packet,http}, 
                                             {reuseaddr,true}]),
    accept_loop(ListenSock, PeerData).

%%--------------------------------------------------------------------------------
%% In-Memory Storage
%%--------------------------------------------------------------------------------

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


%% Takes a Peer record and returns a dictionary that can be encoded by the
%% included bencoder.
format_peer(Peer) ->
    PeerId  = Peer#peer.id,
    Address = Peer#peer.address,
    Port    = Peer#peer.port,
    {dict, dict:from_list([{"ip", Address}, {"port", Port}, {"peer id", PeerId}])}.

%%--------------------------------------------------------------------------------
%% Socket Handling
%%--------------------------------------------------------------------------------

%% Listens for connections on a tcp socket. Each request is dispatched into a
%% new actor.
accept_loop(ListenSock, PeerData) ->	
    {ok, Sock} = gen_tcp:accept(ListenSock),
    spawn(?MODULE, request_handler, [Sock, PeerData]),
    accept_loop(ListenSock, PeerData).


%% Takes a TCP socket and receives a GET request. All the other requests return
%% an error. 
%% See http://erlang.org/doc/man/erlang.html#decode_packet-3
request_handler(Sock, PeerData) ->
    %% Receive the request.
    {ok, {http_request, Method, Path, _Version}} = gen_tcp:recv(Sock, 0),

    %% Fetch the address from the socket and parse it into a string.
    {ok, {AddressStruct, _Port}} = inet:peername(Sock),
    Address = inet_parse:ntoa(AddressStruct),

    case (Method) of
        'GET' ->
            handle_get(Sock, Path, PeerData, Address);
        _ -> 
            send_unsupported_error(Sock)
    end.

%%--------------------------------------------------------------------------------
%% Request Handling
%%--------------------------------------------------------------------------------


%%Todo Fix the other cases. Should suffice for development.
handle_get(Sock, ReqPath, PeerData, Address) ->
    UrlParams = case ReqPath of
                    {abs_path, Path} ->
                        %% Split to get parameters
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        httpd:parse_query(Params);

                    {absoluteURI, http, _Host, _, Path} ->
                        %% Split to get parameters
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        httpd:parse_query(Params);

                    {absoluteURI, _Other_method, _Host, _, _Path} ->
                        send_unsupported_error(Sock);

                    {scheme, _Scheme, _RequestString} ->
                        send_unsupported_error(Sock);

                    _  ->
                        send_forbidden_error(Sock)
                end,
    process_announce(UrlParams, PeerData, Address),
    send_accept(Sock, PeerData).

%%--------------------------------------------------------------------------------
%% Announce Handling
%%--------------------------------------------------------------------------------

%% Handles the announce. The parameters are parsed into a Peer record and then
%% the internal database is updated with the information from this announce.
process_announce(Announce, PeerData, Address) ->
    Infohash       = proplists:get_value("info_hash", Announce),
    PeerId         = proplists:get_value("peer_id", Announce),
    {Port, _}      = string:to_integer(proplists:get_value("port", Announce)),
    {Left, _}      = string:to_integer(proplists:get_value("left", Announce)),
    Event          = proplists:get_value("event", Announce),
    Key            = proplists:get_value("key", Announce, "nokey"),
    io:fwrite("Announce data:~n~p~n", [Announce]),

    IsSeeder = Left > 0,
    %%PeerIdentifier = PeerId++Infohash,
    PeerIdentifier = base64:encode(PeerId++Infohash),

    Peer = #peer{id=PeerId,port=Port,isseeder=IsSeeder,
                 address=Address,identifier=PeerIdentifier,key=Key},

    case Event of
        "stopped" ->
            PeerData ! {remove, Peer#peer.identifier};
        _  ->
            PeerData ! {insert, Peer#peer.identifier, Peer}
    end,
    ok.


%%-------------------------------------------------------------------------------
%% Response Handling
%% ------------------------------------------------------------------------------

%% Builds an HTTP response that contains a bencoded list of peers and minimal
%% bitorrent response data.
build_response(PeerData) ->
    PeerData ! {get_peers, self()},
    receive 
        {ok, {Seeders,Leechers}} ->
            SeedCount = length(Seeders),
            LeechCount = length(Leechers),

            Bencoded = bencode:encode({dict, dict:from_list([{"interval", 60},
                                                             {"min interval", 0},
                                                             {"complete", SeedCount}, 
                                                             {"incomplete", LeechCount}, 
                                                             {"peers", {list, lists:append(Seeders,Leechers)}}])}),
            io:fwrite("Seed count:~p~nLeech count:~p~n", [SeedCount, LeechCount]),
            ContentLength = integer_to_binary(byte_size(Bencoded)),
            [<<"HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\nContent-Length ">>, 
             ContentLength, 
             <<"\r\n">>, 
             Bencoded]
    end.





%% Given a socket it will reply with the list of peers bencoded.
send_accept(Sock, PeerData) ->
    io:fwrite("Sending accept~n"),
    Response = build_response(PeerData),
    io:fwrite("~ts~n", [Response]),
    gen_tcp:send(Sock, Response),
    gen_tcp:close(Sock).

%% gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
%% gen_tcp:close(Sock).


send_unsupported_error(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).


send_forbidden_error(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 403 Forbidden\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).	


%%--------------------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------------------


get_content_length(Sock) ->
    case gen_tcp:recv(Sock, 0, 60000) of
        {ok, {http_header, _, 'Content-Length', _, Length}} -> 
            list_to_integer(Length);
        {ok, {http_header, _, _Header, _, _}}  -> 
            get_content_length(Sock)
    end.


get_body(Sock, Length) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, http_eoh} ->
            inet:setopts(Sock, [{packet, raw}]),
            {ok,Body}=gen_tcp:recv(Sock, Length),
            Body;
        _ ->
            get_body(Sock, Length)
    end.
