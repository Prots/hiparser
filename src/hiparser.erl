-module(hiparser).
-author("prots.igor@gmail.com").

-include("hiparser.hrl").

%% API
-export([
    start/0,
    stop/0,

    parse/1
]).

-record(result, {
    mentions = [],
    emoticons = [],
    links = []
}).

%% API
start() ->
    start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% Main function for parsing input string and preparing JSON response
-spec parse(binary()) -> binary().
parse(String) ->
    Result = binary_parsing(String, #result{}),
    ZippedResult = lists:zip(record_info(fields, result), tl(tuple_to_list(Result))),
    FilteredResult = [{atom_to_binary(Key, utf8), lists:reverse(Value)} || {Key, Value} <- ZippedResult, Value =/= []],
    jsx:encode(FilteredResult).

%% INTERNAL
binary_parsing(<<>>, Result) ->
    Result;
binary_parsing(<<"@", Rest/binary>>, Result) ->
    {UserName, NewRest} = parse_mention(Rest),
    binary_parsing(NewRest, Result#result{mentions = [UserName | Result#result.mentions]});
binary_parsing(<<"(", Rest/binary>>, Result) ->
    case parse_maybe_emoticon(Rest) of
        not_an_emoticon -> binary_parsing(Rest, Result);
        {Emoticon, NewRest} -> binary_parsing(NewRest, Result#result{emoticons = [Emoticon | Result#result.emoticons]})
    end;
binary_parsing(<<"http", _/binary>> = BinaryData, Result) ->
    {Url, Title, NewRest} = parse_link(BinaryData),
    binary_parsing(NewRest, Result#result{links = [[{<<"url">>, Url}, {<<"title">>, Title}] | Result#result.links]});
binary_parsing(<<PrevSymb:1/binary, "@", Rest/binary>>, Result) ->
    case lists:member(PrevSymb, ?NON_PRINTING_SYMBOLS) of
        true ->
            {UserName, NewRest} = parse_mention(Rest),
            binary_parsing(NewRest, Result#result{mentions = [UserName | Result#result.mentions]});
        false ->
            binary_parsing(Rest, Result)
    end;
binary_parsing(<<PrevSymb:1/binary, "(", Rest/binary>>, Result) ->
    case lists:member(PrevSymb, ?NON_PRINTING_SYMBOLS) of
        true ->
            case parse_maybe_emoticon(Rest) of
                not_an_emoticon ->
                    binary_parsing(Rest, Result);
                {Emoticon, NewRest} ->
                    binary_parsing(NewRest, Result#result{emoticons = [Emoticon | Result#result.emoticons]})
            end;
        false ->
            binary_parsing(Rest, Result)
    end;
binary_parsing(<<PrevSymb:1/binary, "http", Rest/binary>>, Result) ->
    case lists:member(PrevSymb, ?NON_PRINTING_SYMBOLS) of
        true ->
            {Url, Title, NewRest} = parse_link(<<"http", Rest/binary>>),
            binary_parsing(NewRest, Result#result{links = [[{<<"url">>, Url}, {<<"title">>, Title}] | Result#result.links]});
        false ->
            binary_parsing(Rest, Result)
    end;
binary_parsing(<<_:1/binary, Rest/binary>>, Result) ->
    binary_parsing(Rest, Result).

parse_mention(BinaryData) ->
    read_to_unprint_symbol(BinaryData, <<>>).

%% Get symbols from binary string until we hit non printed symbol
read_to_unprint_symbol(<<>>, Acc) ->
    {Acc, <<>>};
read_to_unprint_symbol(<<Symb:1/binary, Rest/binary>>, Acc) ->
    case lists:member(Symb, ?NON_PRINTING_SYMBOLS) of
        true -> {Acc, Rest};
        false -> read_to_unprint_symbol(Rest, <<Acc/binary, Symb/binary>>)
    end.

parse_maybe_emoticon(BinaryData) ->
    parse_maybe_emoticon(BinaryData, <<>>, 0).

parse_maybe_emoticon(<<")", Rest/binary>>, Emoticon, Length) when Length =< 15 ->
    %%TODO check emoticon in the cache if we have particular list of them. We can obtain the list of emoticons using HipChat API
    {Emoticon, Rest};
parse_maybe_emoticon(<<_:1/binary, _/binary>>, _Emoticon, Length) when Length > 15 ->
    not_an_emoticon;
parse_maybe_emoticon(<<Symb:1/binary, Rest/binary>>, Emoticon, Length) ->
    parse_maybe_emoticon(Rest, <<Emoticon/binary, Symb/binary>>, Length + 1).

%% Parse input binary string to find URL
parse_link(BinaryData) ->
    {MaybeUrl, RestBinary} = read_to_unprint_symbol(BinaryData, <<>>),
    Title = get_title(MaybeUrl),
    {MaybeUrl, Title, RestBinary}.

%% Check in the cache if Url and Title exist
get_title(Url) ->
    case hiparser_cache:lookup_link(Url) of
        undefined -> do_http_request(Url);
        {ok, Title} -> Title
    end.

%% Parse response body to get Title
parse_body(<<>>) ->
    <<>>;
parse_body(<<"<title>", RestBinary/binary>>) ->
    read_title(RestBinary, <<>>);
parse_body(<<_:1/binary, RestBinary/binary>>) ->
    parse_body(RestBinary).

%% Get whole title
%% TODO We can obtain only particular number of symbols of the title for example first 100 and that's it
read_title(<<>>, _) ->
    <<>>;
read_title(<<"</title>", _/binary>>, Title) ->
    Title;
read_title(<<Symb:1/binary, RestBinary/binary>>, Title) ->
    read_title(RestBinary, <<Title/binary, Symb/binary>>).

%% Make http request to URL to get Title of the page
do_http_request(Url) ->
    case httpc:request(binary_to_list(Url)) of
        {ok, {_StatusCode, _Headers, ResponseBody}} ->
            Title = parse_body(list_to_binary(ResponseBody)),
            ok = hiparser_cache:save_link(Url, Title),
            Title;
        {error, ErrorMsg} ->
            io:format("Response from ~p, Error: ~p~n", [Url, ErrorMsg]),
            <<>>
    end.

%% Load all deps at start
start(AppName) ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    ok = load(AppName),
    {ok, Dependencies} = application:get_key(AppName, applications),
    [begin
         ok = start(A)
     end || A <- Dependencies, not lists:member(A, RunningApps)],
    ok = application:start(AppName).

load(AppName) ->
    F = fun({App, _, _}) -> App end,
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(AppName, LoadedApps) of
        true ->
            ok;
        false ->
            ok = application:load(AppName)
    end.