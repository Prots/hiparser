-module(hiparser).
-author("prots.igor@gmail.com").

%% API
-export([
    start/0,
    stop/0,

    parse/1
]).

-define(NON_PRINTING_SYMBOLS,
    [<<"\0">>, <<"\b">>, <<"\t">>, <<"\n">>, <<"\v">>, <<"\f">>, <<"\r">>, <<"\e">>, <<" ">>]).

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

parse(String) ->
    Result = binary_parsing(String, #result{}),
    ZippedResult = lists:zip(record_info(fields, result), tl(tuple_to_list(Result))),
    FilteredResult = [{atom_to_binary(Key, utf8), lists:reverse(Value)} || {Key, Value} <- ZippedResult, Value =/= []],
    jsx:encode(FilteredResult).

%% INTERNAL
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
    %%TODO check emoticon in the cache
    {Emoticon, Rest};
parse_maybe_emoticon(<<_:1/binary, _/binary>>, _Emoticon, Length) when Length > 15 ->
    not_an_emoticon;
parse_maybe_emoticon(<<Symb:1/binary, Rest/binary>>, Emoticon, Length) ->
    parse_maybe_emoticon(Rest, <<Emoticon/binary, Symb/binary>>, Length + 1).

parse_link(BinaryData) ->
    {MaybeUrl, RestBinary} = read_to_unprint_symbol(BinaryData, <<>>),
    Title = get_title(MaybeUrl),
    {MaybeUrl, Title, RestBinary}.

get_title(Url) ->
    io:format("Request to: ~p~n", [Url]),
    case httpc:request(binary_to_list(Url)) of
        {ok, {StatusCode, _Headers, ResponseBody}} ->
            io:format("Response from ~p, code: ~p~n", [Url, StatusCode]),
            parse_body(list_to_binary(ResponseBody));
        {error, ErrorMsg} ->
            io:format("Response from ~p, Error: ~p~n", [Url, ErrorMsg]),
            <<>>
    end.

parse_body(<<>>) ->
    <<>>;
parse_body(<<"<title>", RestBinary/binary>>) ->
    read_title(RestBinary, <<>>);
parse_body(<<_:1/binary, RestBinary/binary>>) ->
    parse_body(RestBinary).

read_title(<<>>, _) ->
    <<>>;
read_title(<<"</title>", _/binary>>, Title) ->
    Title;
read_title(<<Symb:1/binary, RestBinary/binary>>, Title) ->
    read_title(RestBinary, <<Title/binary, Symb/binary>>).

