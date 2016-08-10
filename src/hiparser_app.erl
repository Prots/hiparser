-module(hiparser_app).
-author("prots.igor@gmail.com").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = hiparser_sup:start_link(),
    ok = hiparser_cache:init(),
    Res.

stop(_State) ->
    ok.
