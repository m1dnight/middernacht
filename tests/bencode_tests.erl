-module(bencode_tests).
-include_lib("eunit/include/eunit.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% An integer is encoded as i<integer encoded in base ten ASCII>e.                 %%
%%                                                                                 %%
%%  - Leading zeros are not allowed (although the number zero is still represented %%
%%    as "0").                                                                     %%
%%                                                                                 %%
%%  - Negative values are encoded by prefixing the number with a minus             %%
%%    sign.                                                                        %%
%%                                                                                 %%
%%  - Negative zero is not permitted.                                              %%
%%                                                                                 %%
%% The number 42 would thus be encoded as i42e, 0 as i0e, and -42 as               %%
%% i-42e.                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%===============================================================================
%% Encoding
%%===============================================================================


integer_encode_test() ->
    <<"i42e">> = bencode:encode(42),
    <<"i0e">> = bencode:encode(0),
    <<"i0e">> = bencode:encode(-0),
    <<"i-42e">> = bencode:encode(-42).


%%===============================================================================
%% Decoding
%%===============================================================================


integer_decode_test() ->
    42 = bencode:decode(<<"i42e">>),
    0 = bencode:decode(<<"i0e">>),
    ?assertError(badarg, bencode:decode(<<"i00e">>)),
    ?assertError(badarg, bencode:decode(<<"i-0e">>)),
    -42 = bencode:decode(<<"i-42e">>).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A byte string (a sequence of bytes, not necessarily characters) is encoded as %%
%% <length>:<contents>.                                                          %%
%%                                                                               %%
%%  - The length is encoded in base 10, like integers, but must be non-negative  %%
%%    (zero is allowed);                                                         %%
%%                                                                               %%
%%  - The contents are just the bytes that make up the string.                   %%
%%                                                                               %%
%% The string "spam" would be encoded as 4:spam.                                 %%
%%                                                                               %%
%% The specification does not deal with encoding of characters outside the ASCII %%
%% set.                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%===============================================================================
%% Encoding
%%===============================================================================


string_encode_test() ->
    <<"4:spam">> = bencode:encode(<<"spam">>),
    <<"1:s">> = bencode:encode(<<"s">>),
    <<"0:">> = bencode:encode(<<"">>).
%%?assertError(badarg, bencode:encode("spamfoobarλ")).


bytes_encode_test() ->
    ABinaryThing = <<250, 240, 134, 135>>,
    <<52,58,250,240,134,135>> = bencode:encode(ABinaryThing),
    <<48, 58>> = bencode:encode(<<>>).


%%===============================================================================
%% Decoding
%%=============================================================================== 

string_decode_test() ->
    <<"spam">> = bencode:decode(<<"4:spam">>),
    <<"s">> = bencode:decode(<<"1:s">>),
    <<"">> = bencode:decode(<<"0:">>).


bytes_decode_test() ->
    <<250, 240, 134, 135>> = bencode:decode(<<52,58,250,240,134,135>>),
    %% This test does not succeed because its impossible to determine
    %% whether a string was put in or bytes. String has precedence.
    <<>> = bencode:decode(<<48,58>>).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A list of values is encoded as l<contents>e .                                 %%
%%                                                                               %%
%%  - The contents consist of the bencoded elements of the list, in order,       %%
%%    concatenated.                                                              %%
%%                                                                               %%
%% A list consisting of the string "spam" and the number 42 would be encoded as: %%
%% l4:spami42ee. Note the absence of separators between elements.                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%===============================================================================
%% Encoding
%%===============================================================================


list_encode_test() ->
    <<"l4:spamd3:bar4:spam3:fooi42eee">> = bencode:encode({list, ["spam", {dict, dict:from_list([{"bar", "spam"}, {"foo", 42}])}]}),
    <<"l4:spami42ee">> = bencode:encode({list, ["spam", 42]}),
    <<"l4:spami42e6:foobare">> = bencode:encode({list, ["spam", 42, "foobar"]}).


%%===============================================================================
%% Decoding
%%=============================================================================== 

list_decode_test() ->
    {list, [<<"spam">>, 42]} = bencode:decode(<<"l4:spami42ee">>),
    {list, []} = bencode:decode(<<"le">>).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A dictionary is encoded as d<contents>e.                                       %%
%%                                                                                %%
%%  - The elements of the dictionary are encoded each key immediately followed by %%
%%    its value.                                                                  %%
%%                                                                                %%
%%  - All keys must be byte strings and must appear in lexicographical            %%
%%    order.                                                                      %%
%%                                                                                %%
%% A dictionary that associates the values 42 and "spam" with the keys "foo" and  %%
%% "bar", respectively (in other words, {"bar": "spam", "foo": 42}}), would be    %%
%% encoded as follows: d3:bar4:spam3:fooi42ee.                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%===============================================================================
%% Encoding
%%===============================================================================


dict_encode_test() ->
    %% Regular test
    <<"d3:bar4:spam3:fooi42ee">> 
        = 
        bencode:encode({dict, dict:from_list([{"bar", "spam"}, {"foo", 42}])}),
    %% Order of keys
    <<"d6:antmani42e5:zorro4:spame">> 
        = 
        bencode:encode({dict, dict:from_list([{"zorro", "spam"}, {"antman", 42}])}).


%%===============================================================================
%% Decoding
%%===============================================================================

dict_decode_test() ->
    {dictionary, [{<<"bar">>, <<"spam">>}, {<<"foo">>, 42}]}  = bencode:decode(<<"d3:bar4:spam3:fooi42ee">>),
    {dictionary, []} = bencode:decode(<<"de">>),
    %% Keys not in lexicographic order
    ?assertError(badarg, bencode:decode(<<"d3:bbb4:spam3:aaai42ee">>)).
