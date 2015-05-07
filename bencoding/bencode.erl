-module(bencode).
-compile(export_all).


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 07/05/2015 13:04:07').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



read_file(FilePath) ->
    FileContent = file:read_file(FilePath), 
    case (FileContent) of
        {ok, Binary} ->
            {Result, _} = debencode(Binary),
            Result;
        {error, Reason} ->
            io:write("Error ~w~n", [Reason])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% THE MAIN DECODING METHODS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debencode(<<$i, Tail/binary>>) ->
    decode_int(Tail, []);

debencode(<<$l, Tail/binary>>) ->
    decode_list(Tail,[]);

debencode(<<$d, Tail/binary>>) ->
    decode_dictionary(Tail,[]);

debencode(Data) ->
    decode_string(Data, []).


%%%% Dictionary %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_dictionary(<<$e, Tail/binary>>, Dictionary) ->
    KeyValues = lists:reverse(Dictionary),
    {{dictionary, KeyValues}, Tail};

decode_dictionary(Data, Dictionary) ->
    % Decode the key, then the value.
    {Key, ValueRest} = decode_string(Data, []),
    {Value, Rest} = debencode(ValueRest),
    decode_dictionary(Rest, [[Key, Value] | Dictionary]).

%%%% List %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_list(<<$e, Tail/binary>>, ListEls) ->
    Elements = lists:reverse(ListEls),
    {{list, Elements}, Tail};

decode_list(Data, ListEls) ->
    {ListEl, Rest} = debencode(Data),
    decode_list(Rest, [ListEl | ListEls]).

%%%% Integer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_int(<<$e, Tail/binary>>, IntBytes) ->
    Integer = list_to_integer(lists:reverse(IntBytes)),
    {{integer, Integer}, Tail};

decode_int(<<Head, Tail/binary>>, IntBytes) ->
    decode_int(Tail, [Head | IntBytes]).

%%%% String %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_string(<<$:, Tail/binary>>, LengthBytes) ->
    Length = list_to_integer(lists:reverse(LengthBytes)),
    <<String:Length/binary, Rest/binary>> = Tail,
    {{string, String}, Rest};

decode_string(<<Head, Tail/binary>>, LengthBytes) ->
    decode_string(Tail, [Head | LengthBytes]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bencode_test() ->
    {{dictionary,[[{string,<<"cow">>},{string,<<"moo">>}],
                  [{string,<<"spam">>},{string,<<"eggs">>}]]},
     <<>>} 
        = debencode(<<"d3:cow3:moo4:spam4:eggse">>),
    {{list,[{string,<<"spam">>},{string,<<"eggs">>},{string,<<"spam">>},{string,<<"eggs">>}]},<<>>} 
        = debencode(<<"l4:spam4:eggs4:spam4:eggse">>),
    {{integer,-3},<<>>} = debencode(<<"i-3e">>),
    {{integer,3},<<>>} = debencode(<<"i3e">>),
    {{string, <<"spam">>},<<>>} = debencode(<<"4:spam">>),

    ok.
