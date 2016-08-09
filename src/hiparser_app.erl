-module(hiparser_app).
-author("prots.igor@gmail.com").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    hiparser_sup:start_link().

stop(_State) ->
    ok.
