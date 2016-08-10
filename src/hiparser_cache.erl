-module(hiparser_cache).
-author("prots.igor@gmail.com").

-include("hiparser.hrl").

%% API
-export([
    init/0,
    save_link/2,
    lookup_link/1
]).

%% API
%% Create cache tables for Links and Emoticons
init() ->
    _ = ets:new(?LINKS_TBL, [public, set, named_table, {read_concurrency, true}]),
    _ = ets:new(?EMOTICONS_TBL, [public, set, named_table, {read_concurrency, true}]),
    ok.

%% Save Url, Page Title and timestamp to ETS
save_link(Url, Title) ->
    true = ets:insert(?LINKS_TBL, {Url, Title, ?TS}),
    ok.

%% Lookup Title by Url
lookup_link(Url) ->
    case ets:lookup(?LINKS_TBL, Url) of
        [] -> undefined;
        [{Url, Title, _}|_] -> {ok, Title}
    end.

%% TODO periodically clean up cache from old records using timestamp
