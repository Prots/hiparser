-module(main_SUITE).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,

    test_mentions/1,
    test_emoticons/1,
    test_links/1,
    test_all/1
]).

all() ->
    [
        test_mentions
        , test_emoticons
        , test_links
        , test_all
    ].

init_per_suite(Config) ->
    ok = hiparser:start(),
    Config.

end_per_suite(Config) ->
    ok = hiparser:stop(),
    Config.

%% TESTS

test_mentions(_) ->
    make_test("mentions").

test_emoticons(_) ->
    make_test("emoticons").

test_links(_) ->
    make_test("links").

test_all(_) ->
    make_test("all").

%% Internal
get_main_path() ->
    _ = shell_default:cd(".."),
    _ = shell_default:cd(".."),
    {ok, MainPath} = file:get_cwd(),
    MainPath.

make_test(TestName) ->
    MainPath = get_main_path(),
    DataDirPath = MainPath ++  "/test/test_data/" ++ TestName ++ "/",
    {ok, ListDir} = file:list_dir(DataDirPath),
    [begin
         {ok, InputData} = file:read_file(DataDirPath ++ TestDir ++ "/input.txt"),
         {ok, OutPutData} = file:read_file(DataDirPath ++ TestDir ++ "/output.json"),
         ParsedData = hiparser:parse(InputData),
         ct:print("Input: ~p~nWant: ~p~nResult: ~p~n", [InputData, jsx:minify(OutPutData), ParsedData]),
         ?assertEqual(jsx:minify(OutPutData), ParsedData)
     end || TestDir <- lists:sort(ListDir)].

