-module(mb2_bencoding).
-author("Tim Carey-Smith <tim@spork.in>").
-vsn("1.0").

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([encode/1, decode/1, parse/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: encode/1
%% Description: Encode an erlang term into a String
%%--------------------------------------------------------------------
encode({string, Str}) ->
    L = length(Str),
    {ok, [integer_to_list(L), ":", Str]};

encode({integer, Int}) ->
    {ok, ["i", integer_to_list(Int), "e"]};

encode({list, Items}) ->
    {ok, EncodedItems} = encode_list(Items),
    {ok, ["l", EncodedItems, "e"]};

encode({dict, Items}) ->
    {ok, EncodedItems} = encode_dict(Items),
    {ok, ["d", EncodedItems, "e"]};

encode(Term) ->
    ?ERR("Can not encode term: ~p~n", [Term]),
    {error, invalid_structure}.

%%--------------------------------------------------------------------
%% Function: decode/1
%% Description: Decode a string to an erlang term.
%%--------------------------------------------------------------------
decode(String) ->
    case decode_b(String) of
        {ok, Result, []} ->
            {ok, Result};
        {ok, _Result, Rest} ->
            ?ERR("Unparsed data: ~p~n", [Rest]),
            {error, extra_data_given};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: parse/1
%% Description: Parse a file into an erlang term.
%%--------------------------------------------------------------------
parse(FileName) ->
    case file:read_file(FileName) of
        {ok, Data} ->
            String = binary_to_list(Data),
	    decode(String);
        E ->
            E
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% encoding
encode_list(Items) ->
    EncodedItems = lists:map(fun (I) ->
                                     {ok, Item} = encode(I),
                                     Item
                             end, Items),
    {ok, EncodedItems}.

encode_dict(Items) ->
    {ok, EncodedItems} = encode_dict_recurse(Items, []),
    {ok, lists:reverse(EncodedItems)}.

encode_dict_recurse([], EncodedItems) ->
    {ok, EncodedItems};

encode_dict_recurse([{K1, V1} | Rest], Accum) ->
    {ok, K} = encode(K1),
    {ok, V} = encode(V1),
    encode_dict_recurse(Rest, [V, K | Accum]).

%% decoding
decode_b(String) ->
    First = hd(String),
    case First of
        $i ->
            decode_integer(String);
	$l ->
	    decode_list(String);
	$d ->
	    decode_dict(String);
	_S ->
	    decode_string(String)
    end.


decode_integer([$i | String]) ->
    case split_on($e, String) of
        {ok, IntegerString, Rest} ->
            case string:to_integer(IntegerString) of
                {error, Reason} ->
                    {error, Reason};
                {Integer, _} ->
                    {ok, {integer, Integer}, Rest}
            end;
        E ->
            E
    end.

decode_list([$l | String]) ->
    {ok, Items, Rest} = decode_list_recurse(String, []),
    {ok, {list, Items}, Rest}.

decode_list_recurse([$e | Rest], Items) ->
    {ok, lists:reverse(Items), Rest};
decode_list_recurse(String, Items) ->
    {ok, Item, Rest} = decode_b(String),
    decode_list_recurse(Rest, [Item | Items]).

decode_dict([$d | String]) ->
    {ok, Items, Rest} = decode_list_recurse(String, []),
    {ok, Dict} = items_to_dict(Items, []),
    {ok, {dict, Dict}, Rest}.

items_to_dict([], Dict) ->
    {ok, lists:reverse(Dict)};
items_to_dict([K, V | Rest], Dict) ->
    items_to_dict(Rest, [{K, V} | Dict]).

decode_string(String) ->
    {ok, LengthString, Data} = split_on($:, String),
    {Length, _} = string:to_integer(LengthString),
    {Content, Rest} = lists:split(Length, Data),
    {ok, {string, Content}, Rest}.

split_on(Char, String) ->
    case lists:splitwith(char_pred(Char), String) of
        {Match, [Char | Rest]} ->
            {ok, Match, Rest};
        _ ->
            ?ERR("Terminator ~c not found~n", [Char]),
            {error, terminator_not_found}
    end.

char_pred(C) ->
    fun(E) ->
	    E /= C
    end.

%% Tests
flatten_encode(Term) ->
    case encode(Term) of
        {ok, IOList} ->
            {ok, lists:flatten(IOList)};
        E ->
            E
    end.

encode_integer_test() ->
    {ok, "i100e"} = flatten_encode({integer, 100}).
encode_string_empty_test() ->
    {ok, "0:"} = flatten_encode({string, ""}).
encode_string_hello_test() ->
    {ok, "5:hello"} = flatten_encode({string, "hello"}).
encode_list_empty_test() ->
    {ok, "le"} = flatten_encode({list, []}).
encode_list_test() ->
    {ok, "l4:spam4:eggse"} = flatten_encode({list, [{string, "spam"}, {string, "eggs"}]}).
encode_list_2_test() ->
    {ok, "l3:onei2ee"} = flatten_encode({list, [{string, "one"}, {integer, 2}]}).
encode_dict_empty_test() ->
    {ok, "de"} = flatten_encode({dict, []}).
encode_dict_test() ->
    {ok, "d3:cow3:moo4:spam4:eggse"} = flatten_encode({dict, [{{string, "cow"}, {string, "moo"}}, {{string, "spam"}, {string, "eggs"}}]}).
encode_unknown_test() ->
    {error, invalid_structure} = flatten_encode({wont, work}).

decode_raw_integer_test() ->
    {ok, {integer, 100}, []} = decode_integer("i100e").
decode_raw_integer_invalid_test() ->
    {error, terminator_not_found} = decode_integer("i100").
decode_integer_test() ->
    {ok, {integer, 100}} = decode("i100e").
decode_string_test() ->
    {ok, {string, "spam"}} = decode("4:spam").
decode_list_empty_test() ->
    {ok, {list, []}} = decode("le").
decode_list_test() ->
    {ok, {list, [{integer, 30}, {integer, 2}]}} = decode("li30ei2ee").
decode_dict_empty_test() ->
    {ok, {dict, []}} = decode("de").
decode_dict_test() ->
    {ok, {dict, [{{integer, 30}, {integer, 2}}, {{integer, 102}, {integer, 4}}]}} = decode("di30ei2ei102ei4ee").
decode_dict_2_test() ->
    {ok, {dict, [{{string, "moo"}, {integer, 2}}, {{string, "spam"}, {integer, 4}}]}} = decode("d3:mooi2e4:spami4ee").

encode_decode_dict_test() ->
    String = "di30ei2ei102ei4ee",
    {ok, Term} = decode(String),
    {ok, String} = flatten_encode(Term).
encode_decode_crazy_dict_test() ->
    String = "d3:cow3:moo4:spam4:eggse",
    {ok, Term} = decode(String),
    {ok, String} = flatten_encode(Term).
