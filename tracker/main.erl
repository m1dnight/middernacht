-module(main).
-compile(export_all).


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 1995/01/05 13:04 13:04:07 ').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Port)->
    {ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
    loop(ListenSock).


loop(ListenSock) ->	
    {ok, Sock}=gen_tcp:accept(ListenSock),
    spawn(?MODULE, handle_request, [Sock]),
    loop(ListenSock).

%% Takes a TCP socket and receives 
%% http://erlang.org/doc/man/erlang.html#decode_packet-3
handle_request(Sock) ->
    {ok, {http_request, Method, Path, _Version}} = gen_tcp:recv(Sock, 0),

    case (Method) of
        'POST' ->
            handle_post(Sock);
        'GET' ->
            handle_get(Sock, Path);
        _ -> 
            send_unsupported_error(Sock)
    end.


handle_post(Sock) ->
    Length=get_content_length(Sock),
    PostBody=get_body(Sock, Length),
    io:fwrite("~s~n", [PostBody]),
    send_accept(Sock).

%TODO Fix the other cases. Should suffice for development.
handle_get(Sock, ReqPath) ->
    UrlParams = case ReqPath of
                    {abs_path, Path} ->
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        ParsedParms = httpd:parse_query(Params),
                        ParsedParms;
                    {absoluteURI, http, _Host, _, Path} ->
                        Params = string:substr(Path, string:str(Path, "?") + 1),
                        ParsedParms = httpd:parse_query(Params),
                        ParsedParms;
                    {absoluteURI, _Other_method, _Host, _, _Path} ->
                        send_unsupported_error(Sock);
                    {scheme, _Scheme, _RequestString} ->
                        send_unsupported_error(Sock);
                    _  ->
                        send_forbidden_error(Sock)
                    end,
    
    %% End Debugging
    send_accept(Sock).


send_accept(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).

send_unsupported_error(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).

send_forbidden_error(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 403 Forbidden\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).	





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
