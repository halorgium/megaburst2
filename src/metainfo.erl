%% @author Tim Carey-Smith <dev@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc builds a metainfo record
%% @end
%%%-------------------------------------------------------------------
-module(metainfo).

-include_lib("logging.hrl").
-include_lib("records.hrl").
-include_lib("eunit.hrl").

%% API
-export([read/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: read/1
%% Description: 
%%--------------------------------------------------------------------
read(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            String = binary_to_list(Data),
            {ok, Term} = bencoding:decode(String),
            term_to_metainfo(Term);
        E ->
            E
    end.

%%====================================================================
%% Internal functions
%%====================================================================

term_to_metainfo(Term) ->
    {ok, Announce} = announce(Term),
    {ok, InfoHash} = info_hash(Term),
    #metainfo{info_hash = InfoHash, announce = Announce, raw = Term}.

info_hash(Term) ->
    {ok, InfoDict} = fetch_value("info", Term),
    {ok, InfoString} = bencoding:encode({dict, InfoDict}),
    {ok, crypto:sha(list_to_binary(InfoString))}.

announce(Term) ->
    fetch_value("announce", Term).

fetch_value(Key, Metainfo) ->
    case Metainfo of
        {dict, Dict} ->
            Search = {string, Key},
            {value, {Search, {_, Value}}} = lists:keysearch(Search, 1, Dict),
            {ok, Value};
        _ ->
            {error, not_a_dict}
    end.
