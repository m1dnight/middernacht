-module(main).
-compile(export_all).


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 1995/01/05 13:04 13:04:07 ').

-record(peer, {address, id, port, infohash, key, isseeder}).

%%--------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------

start(Port)->
    {ok, ListenSock} = gen_tcp:listen(Port, [list,{active, false},{packet,http}, {reuseaddr,true}]),
    PeerData = spawn(?MODULE, peerdata, [dict:new()]),
    loop(ListenSock, PeerData).

%%--------------------------------------------------------------------------------
%% In-Memory Storage
%%--------------------------------------------------------------------------------
peerdata(Peers) ->
    receive 
        {insert, Identifier, Values} ->
            io:fwrite("Adding peer ~p~ninfo: ~p~n", [Identifier, Values]),
            NewDict = dict:update(Identifier, fun(_) -> Values end, Values, Peers),
            peerdata(NewDict);

        {fetch, Identifier, Sender} ->
            Fetch = dict:find(Identifier, Peers),
            case Fetch of
                {ok, Values} ->
                    Sender ! Values;
                _ ->
                    Sender ! {notfound}
            end;

        {remove, Identifier} ->
            io:fwrite("Removing peer ~p~n", [Identifier]),
            NewDict = dict:erase(Identifier, Peers),
            peerdata(NewDict);

        {bencodepeers, Sender} ->
            PeerList = dict:fold(fun(_Id, Value, Ps) -> Bencd = bencode_peer(Value), [Ps | Bencd] end , [],  Peers),
            Sender ! {ok, PeerList}
    end,
    peerdata(Peers).


bencode_peer(Peer) ->
    _PeerId = Peer#peer.id,
    Address = Peer#peer.address,
    Port = Peer#peer.port,
    {dictionary, {"ip", Address}, {"port", Port}}.

%%--------------------------------------------------------------------------------
%% Socket Handling
%%--------------------------------------------------------------------------------

loop(ListenSock, PeerData) ->	
    {ok, Sock} = gen_tcp:accept(ListenSock),
    spawn(?MODULE, handle_request, [Sock, PeerData]),
    loop(ListenSock, PeerData).

%% Takes a TCP socket and receives 
%% http://erlang.org/doc/man/erlang.html#decode_packet-3
handle_request(Sock, PeerData) ->
    {ok, {http_request, Method, Path, _Version}} = gen_tcp:recv(Sock, 0),

    case (Method) of
        'GET' ->
            handle_get(Sock, Path, PeerData);
        _ -> 
            send_unsupported_error(Sock)
    end.

%%--------------------------------------------------------------------------------
%% Request Handling
%%--------------------------------------------------------------------------------


%%TODO Fix the other cases. Should suffice for development.
handle_get(Sock, ReqPath, PeerData) ->
    UrlParams = case ReqPath of
                    {abs_path, Path} ->
                        %% Split to get parameters
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        httpd:parse_query(Params);

                    {absoluteURI, http, _Host, _, Path} ->
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        _ParsedParms = httpd:parse_query(Params);

                    {absoluteURI, _Other_method, _Host, _, _Path} ->
                        send_unsupported_error(Sock);

                    {scheme, _Scheme, _RequestString} ->
                        send_unsupported_error(Sock);

                    _  ->
                        send_forbidden_error(Sock)
                end,
    handle_announce(UrlParams, PeerData),
    %% End Debugging
    send_accept(Sock, PeerData).

%%--------------------------------------------------------------------------------
%% Announce Handling
%%--------------------------------------------------------------------------------
handle_announce(Parameters, PeerData) ->
    PeerId = proplists:get_value("peer_id", Parameters),
    Port   = proplists:get_value("port", Parameters),
    Hash   = proplists:get_value("info_hash", Parameters),
    Key    = proplists:get_value("key", Parameters),
    Event  = proplists:get_value("event", Parameters),
    io:fwrite("Announce : ~p~n", [Parameters]),
    Identifier = PeerId++Hash,

    Peer = #peer{address="", id=PeerId, port=Port, infohash=Hash, key=Key, isseeder=false},

    case Event of
        "stopped" ->
            io:fwrite("~p stopped torrent ~p~n", [PeerId, Hash]),
            PeerData ! {remove, Identifier};
        _  ->
            io:fwrite("Updated information for ~p~n", [PeerId]),
            PeerData ! {insert, Identifier, Peer}
    end,
    ok.




%%--------------------------------------------------------------------------------
%% Response Handling
%%--------------------------------------------------------------------------------
%% Tracker Response

%% The tracker responds with "text/plain" document consisting of a bencoded dictionary with the following keys:

%%     failure reason: If present, then no other keys may be present. The value
%%     is a human-readable error message as to why the request failed (string).

%%     warning message: (new, optional) Similar to failure reason, but the
%%     response still gets processed normally. The warning message is shown just
%%     like an error.

%%     interval: Interval in seconds that the client should wait between sending
%%     regular requests to the tracker

%%     min interval: (optional) Minimum announce interval. If present clients
%%     must not reannounce more frequently than this.

%%     tracker id: A string that the client should send back on its next
%%     announcements. If absent and a previous announce sent a tracker id, do
%%     not discard the old value; keep using it.

%%     complete: number of peers with the entire file, i.e. seeders (integer)

%%     incomplete: number of non-seeder peers, aka "leechers" (integer)

%%     peers: (dictionary model) The value is a list of dictionaries, each with
%%     the following keys:

%%         peer id: peer's self-selected ID, as described above for the tracker
%%         request (string)

%%         ip: peer's IP address either IPv6 (hexed) or IPv4 (dotted quad) or
%%         DNS name (string)

%%         port: peer's port number (integer)

%%     peers: (binary model) Instead of using the dictionary model described
%%     above, the peers value may be a string consisting of multiples of 6
%%     bytes. First 4 bytes are the IP address and last 2 bytes are the port
%%     number. All in network (big endian) notation.


send_accept(Sock, PeerData) ->
    PeerData ! {bencodepeers, self()},
    receive 
        {ok, PeerList} ->
            io:fwrite("Peerlist: ~p~n", [PeerList]),
            Bencoded = bencode:encode({dictionary, dict:from_list([{"interval", 1},{"min interval", 1},{"complete", 0}, {"incomplete", 0}, {"peers", {list, PeerList}}])}),
            io:fwrite("bencoded response:~p~n", [Bencoded]),
            ContentLength = byte_size(Bencoded),
            RespData = [<<"HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\nContent-Length ">>, ContentLength, <<"\r\n">>, Bencoded],
            io:fwrite("response:~p~n", [RespData]),
            gen_tcp:send(Sock, RespData)
    end.

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
