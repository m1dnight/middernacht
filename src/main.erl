-module(main).
-export([init/1]).
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
%% - Monitor storage and respawn upon failure.

%%--------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------

init(Port) ->
    storage:init(),
    {ok, ListenSock} = gen_tcp:listen(Port, [list,
                                             {active, false},
                                             {packet,http}, 
                                             {reuseaddr,true}]),
    spawn(?MODULE, accept_loop, [ListenSock]).
    %%accept_loop(ListenSock).


%%--------------------------------------------------------------------------------
%% Socket Handling
%%--------------------------------------------------------------------------------


%% Listens for connections on a tcp socket. Each request is dispatched into a
%% new actor.
accept_loop(ListenSock) ->
    Result = gen_tcp:accept(ListenSock),
    io:fwrite("Accepted on socekt~n", []),
    case Result of
         {ok, Sock} ->
            spawn(?MODULE, request_handler, [Sock]),
            accept_loop(ListenSock);
        _  ->
            error_logger:error_report(io_lib:format("Error accepting on socket! ~p~n", [Result]))
    end.


%% Takes a TCP socket and receives a GET request. All the other requests return
%% an error. 
%% See http://erlang.org/doc/man/erlang.html#decode_packet-3
request_handler(Sock) ->
    %% Receive the request.
    Result = gen_tcp:recv(Sock, 0),
    case Result of
        %% Not sure which cases to handle here. HttpError?
        {ok, {http_request, Method, Path, _Version}} ->
            case (Method) of
                'GET' ->
                    handle_get(Sock, Path);
                _ -> 
                    send_unsupported_error(Sock)
            end;

        {error, _Reason} ->
            error_logger:error_report("Error receiving from socket. Aborting.");

        Other  ->
            error_logger:error_report(io_lib:format("Received unknown data, aborting.~p~n", [Other]))
    end.


%%--------------------------------------------------------------------------------
%% Request Handling
%%--------------------------------------------------------------------------------


%%Todo Fix the other cases. Should suffice for development.
handle_get(Sock, ReqPath) ->
    UrlParams = case ReqPath of
                    {abs_path, Path} ->
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        httpd:parse_query(Params);
                    {absoluteURI, http, _Host, _, Path} ->
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        httpd:parse_query(Params);

                    {absoluteURI, _Other_method, _Host, _, _Path} ->
                        send_unsupported_error(Sock);
                    {scheme, _Scheme, _RequestString} ->
                        send_unsupported_error(Sock);
                    _  ->
                        send_forbidden_error(Sock)
                end,
    io:fwrite("Requested GET path:~n~p~n", [ReqPath]),
    io:fwrite("Announce parameters:~n~p~n", [UrlParams]),
    process_announce(Sock, UrlParams),
    %% Accept the data and return.
    send_accept(Sock).

%%--------------------------------------------------------------------------------
%% Announce Handling
%%--------------------------------------------------------------------------------

%% Handles the announce. The parameters are parsed into a Peer record and then
%% the internal database is updated with the information from this announce.
process_announce(Sock, Announce) ->
    %% Construct peer struct from parameters.
    Infohash   = proplists:get_value("info_hash", Announce),
    PeerId     = proplists:get_value("peer_id", Announce),
    {Port, _}  = string:to_integer(proplists:get_value("port", Announce)),
    {Left, _}  = string:to_integer(proplists:get_value("left", Announce)),
    Event      = proplists:get_value("event", Announce),
    Key        = proplists:get_value("key", Announce, "nokey"),
    Address    = socket_address_string(Sock),
    IsSeeder   = not(Left >  0),
    Identifier = base64:encode(PeerId++Infohash),

    Peer = #peer{id=PeerId,port=Port,isseeder=IsSeeder,
                 address=Address,identifier=Identifier,key=Key},

    case Event of
        "stopped" ->
            storage:remove(whereis(store), Identifier);
        "started" ->
            storage:insert(whereis(store), Identifier, Peer);
        _  ->
            storage:insert(whereis(store), Identifier, Peer)
    end,
    ok.


%%-------------------------------------------------------------------------------
%% Response Handling
%% ------------------------------------------------------------------------------

%% Builds an HTTP response that contains a bencoded list of peers and minimal
%% bitorrent response data.
build_response() ->
    %%store ! {get_peers, self()},
    %% TODO: Maybe it would be better to make the storage reply, or becode the data at least?
    %% Limit the amount of peers?
    {Seeders,Leechers} = storage:get_peers(whereis(store)),

    SeedCount = length(Seeders),
    LeechCount = length(Leechers),
    Bencoded = bencode:encode({dict, dict:from_list([{"interval", 60},
                                                     {"min interval", 0},
                                                     {"complete", SeedCount}, 
                                                     {"incomplete", LeechCount}, 
                                                     {"peers", {list, lists:append(Seeders,Leechers)}}])}),
    ContentLength = integer_to_binary(byte_size(Bencoded)),

    [<<"HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\nContent-Length: ">>, 
     ContentLength, 
     <<"\r\n\r\n">>, 
     Bencoded].

%%--------------------------------------------------------------------------------
%% Responses
%%--------------------------------------------------------------------------------

%% Given a socket it will reply with the list of peers bencoded.
send_accept(Sock) ->
    storage:print_status(whereis(store)),
    Response = build_response(),

    io:fwrite("Sending accept~n"),
    io:fwrite("Full http response:~n~ts~n", [Response]),

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


%% For debugging. Lan ips are translated into wlan ips.
socket_address_string(Socket) ->
    {ok, {AddressStruct, _Port}} = inet:peername(Socket),
    IpStruct = case AddressStruct of
                   {192,_,_,_} ->
                       {81,242,29,47};
                   _ -> AddressStruct
               end,
    inet_parse:ntoa(IpStruct).  

%% socket_address_string(Socket) ->
%%     {ok, {AddressStruct, _Port}} = inet:peername(Socket),
%%     inet_parse:ntoa(AddressStruct). 

