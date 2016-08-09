-module(main_SUITE).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,

    test_mentions/1,
    test_emoticons/1,
    test_links/1
]).

all() ->
    [
        test_mentions
%%        ,test_emoticons,
%%        test_links
    ].

init_per_suite(Config) ->
    ok = hiparser:start(),
    Config.

end_per_suite(Config) ->
    ok = hiparser:stop(),
    Config.

%% TESTS

test_mentions(_) ->
    MainPath = get_main_path(),
    DataDirPath = MainPath ++  "/test/test_data/mentions/",
    {ok, ListDir} = file:list_dir(DataDirPath),
    [begin
         {ok, InputData} = file:read_file(DataDirPath ++ TestDir ++ "/input.txt"),
         {ok, OutPutData} = file:read_file(DataDirPath ++ TestDir ++ "/output.txt"),
         ParsedData = hiparser:parse(InputData),
         ct:print("Input: ~p~nWant: ~p~nResult: ~p~n", [InputData, OutPutData, ParsedData]),
         ?assertEqual(OutPutData, ParsedData)
     end || TestDir <- lists:sort(ListDir)].

test_emoticons(_) ->
    MainPath = get_main_path(),
    DataDirPath = MainPath ++  "/test/test_data/emoticons/",
    {ok, ListDir} = file:list_dir(DataDirPath),
    [begin
         {ok, InputData} = file:read_file(DataDirPath ++ TestDir ++ "/input.txt"),
         {ok, OutPutData} = file:read_file(DataDirPath ++ TestDir ++ "/output.txt"),
         ParsedData = hiparser:parse(InputData),
         ct:print("Input: ~p~nWant: ~p~nResult: ~p~n", [InputData, OutPutData, ParsedData]),
         ?assertEqual(OutPutData, ParsedData)
     end || TestDir <- lists:sort(ListDir)].

test_links(_) ->
    MainPath = get_main_path(),
    DataDirPath = MainPath ++  "/test/test_data/links/",
    {ok, ListDir} = file:list_dir(DataDirPath),
    [begin
         {ok, InputData} = file:read_file(DataDirPath ++ TestDir ++ "/input.txt"),
         {ok, OutPutData} = file:read_file(DataDirPath ++ TestDir ++ "/output.txt"),
         ParsedData = hiparser:parse(InputData),
         ct:print("Input: ~p~nWant: ~p~nResult: ~p~n", [InputData, OutPutData, ParsedData]),
         ?assertEqual(OutPutData, ParsedData)
     end || TestDir <- lists:sort(ListDir)].


%% Internal
get_main_path() ->
    _ = shell_default:cd(".."),
    _ = shell_default:cd(".."),
    {ok, MainPath} = file:get_cwd(),
    MainPath.

