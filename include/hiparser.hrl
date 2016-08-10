-author("prots.igor@gmail.com").

-define(LINKS_TBL, hiparser_links_cache).
-define(EMOTICONS_TBL, hiparser_emoticons_cache).

-define(NON_PRINTING_SYMBOLS,
    [<<"\0">>, <<"\b">>, <<"\t">>, <<"\n">>, <<"\v">>, <<"\f">>, <<"\r">>, <<"\e">>, <<" ">>]).

-define(TS, calendar:datetime_to_gregorian_seconds(calendar:local_time())).