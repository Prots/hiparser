-module(hiparser_cache).
-author("prots.igor@gmail.com").

-include("hiparser.hrl").

-define(TS, calendar:datetime_to_gregorian_seconds(calendar:local_time())).

%% API
-export([
    init/0,
    save_link/2,
    lookup_link/1
]).

%% API
init() ->
    _ = ets:new(?LINKS_TBL, [public, set, named_table, {read_concurrency, true}]),
    _ = ets:new(?EMOTICONS_TBL, [public, set, named_table, {read_concurrency, true}]),
    ok.

save_link(Url, Title) ->
    true = ets:insert(?LINKS_TBL, {Url, Title, ?TS}),
    ok.

lookup_link(Url) ->
    case ets:lookup(?LINKS_TBL, Url) of
        [] -> undefined;
        [{Url, Title, _}|_] -> {Url, Title}
    end.
