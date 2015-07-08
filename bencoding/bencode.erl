-module(bencode).
-compile(export_all).
-compile(debug_info).


-revision('Revision: 1.0 ').
-created('Date: 07/05/2015').
-created_by('christophe.detroyer@gmail.com').
-modified('Date: 07/05/2015 13:04:07').

%%===============================================================================
%% API
%%===============================================================================

%%-------------------------------------------------------------------------------
%% Encoding
%%-------------------------------------------------------------------------------
encode(Struct) ->
    list_to_binary(enc(Struct)).

decode(Input) ->
    {Decoded, _Rest} = debencode(Input),
    Decoded.

%%-------------------------------------------------------------------------------
%% Decoding
%%-------------------------------------------------------------------------------

%% Edge cases:
%% No leading zeros.
debencode(<<$i,$0,$0, _Tail/binary>>) ->
    erlang:error(badarg);

%% No negative zero
debencode(<<$i,$-,$0, _Tail/binary>>) ->
    erlang:error(badarg);

debencode(<<$i, Tail/binary>>) ->
    decode_int(Tail, []);

debencode(<<$l, Tail/binary>>) ->
    decode_list(Tail,[]);

debencode(<<$d, Tail/binary>>) ->
    decode_dictionary(Tail,[]);

debencode(Data) ->
    decode_string(Data, []).


%%===============================================================================
%% Inner functions
%%===============================================================================

%%-------------------------------------------------------------------------------
%% Dictionary decoding
%%-------------------------------------------------------------------------------

decode_dictionary(<<$e, Tail/binary>>, Dictionary) ->
    KeyValues = lists:reverse(Dictionary),
    Keys = lists:map(
	     fun({Key, _Val}) ->
		     Key
	     end, KeyValues),
    SortedKeys = is_sorted(Keys),
    if 
        not(SortedKeys) ->
            erlang:error(badarg);
        true ->
            {{dictionary, KeyValues}, Tail}
    end;




decode_dictionary(Data, Dictionary) ->
    %% Decode the key, then the value.
    {Key, ValueRest} = decode_string(Data, []),
    {Value, Rest} = debencode(ValueRest),
    decode_dictionary(Rest, [{Key, Value} | Dictionary]).


%%-------------------------------------------------------------------------------
%% List decoding
%%-------------------------------------------------------------------------------
decode_list(<<$e, Tail/binary>>, ListEls) ->
    Elements = lists:reverse(ListEls),
    {{list, Elements}, Tail};


decode_list(Data, ListEls) ->
    {ListEl, Rest} = debencode(Data),
    decode_list(Rest, [ListEl | ListEls]).


%%-------------------------------------------------------------------------------
%% Integer decoding
%%-------------------------------------------------------------------------------

decode_int(<<$e, Tail/binary>>, IntBytes) ->
    Integer = list_to_integer(lists:reverse(IntBytes)),
    {Integer, Tail};

decode_int(<<Head, Tail/binary>>, IntBytes) ->
    decode_int(Tail, [Head | IntBytes]).

%%-------------------------------------------------------------------------------
%% String decoding
%%-------------------------------------------------------------------------------

%% Parses the entire string, with the length of the string reversed 
%% in LengthBytes.
decode_string(<<$:, Tail/binary>>, LengthBytes) ->
    Length = list_to_integer(lists:reverse(LengthBytes)), % Compute the length
    <<String:Length/binary, Rest/binary>> = Tail, % Parse out the bytes for the string
    {String, Rest};

%% Parses the length of the string (e.g., 4 in "4:spam")
decode_string(<<Head, Tail/binary>>, LengthBytes) ->
    decode_string(Tail, [Head | LengthBytes]).


%%-------------------------------------------------------------------------------
%% Integer encoding
%%-------------------------------------------------------------------------------

enc(Int) when is_integer(Int) ->
    IntBin = list_to_binary(integer_to_list(Int)),
    [$i, IntBin, $e];

%%-------------------------------------------------------------------------------
%% String encoding
%%-------------------------------------------------------------------------------

enc(Str) when is_list(Str) ->
    enc(list_to_binary(Str));

enc(Str) when is_binary(Str) ->
    IntBin = list_to_binary(integer_to_list(size(Str))),
    [IntBin, $:, Str];

%%-------------------------------------------------------------------------------
%% List encoding
%%-------------------------------------------------------------------------------

enc({list, List}) when is_list(List) ->
    [$l, [enc(Elem) || Elem <- List], $e];

%%-------------------------------------------------------------------------------
%% Dictionary encoding
%%-------------------------------------------------------------------------------

enc({dict, Dict}) ->
    Data = lists:map(
	     fun({Key, Val}) when is_list(Key) or is_binary(Key) ->
		     [enc(Key), enc(Val)]
	     end, lists:keysort(1, dict:to_list(Dict))),
    [$d, Data, $e].


%%-------------------------------------------------------------------------------
%% Helper functions
%%-------------------------------------------------------------------------------

%% Checks if a given list contains ASCII-interpretable bytes only.
ascii_chars(List) ->
    lists:all(fun(Byte) -> Byte < 128 end, List).


%% Checks if a list is in sorted order.
is_sorted([X,Y|Rest]) ->
    X =< Y andalso is_sorted([Y | Rest]);

is_sorted(_) ->
    true.


    

