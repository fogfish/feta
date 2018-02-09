-module(csv_tests).

-include_lib("eunit/include/eunit.hrl").

quotes_and_newlines_test() ->
    % given
    Expected = [[<<"a">>, <<"b">>],
                [<<"1">>, <<"ha \"ha\" ha">>],
                [<<"3">>, <<"4">>]],
    CsvBin = <<"a,b\n1,\"ha \n\"\"ha\"\" \nha\"\n3,4\n">>,
    % when
    {Csv, _} = csv:decode(CsvBin, csv:new()),
    % then
    ?assertEqual(Expected, Csv).


escaped_quotes_test() ->
    % given
    Expected = [[<<"a">>, <<"b">>],
                [<<"1">>, <<"ha \"ha\" ha">>],
                [<<"3">>, <<"4">>]],
    CsvBin = <<"a,b\n1,\"ha \"\"ha\"\" ha\"\n3,4\n">>,
    % when
    {Csv, _} = csv:decode(CsvBin, csv:new()),
    % then
    ?assertEqual(Expected, Csv).

utf8_test() ->
    % given
    Expected = [[<<"a">>,<<13371/utf8>>,<<"c">>],
                [<<"1">>,<<"2">>,<<"3">>],
                [<<"4">>,<<"5">>,<<676/utf8>>]],
    CsvBin = <<97,44,227,144,187,44,99,10,49,44,50,44,51,10,52,44,53,44,202,164,10>>,
    % when
    {Csv, _} = csv:decode(CsvBin, csv:new()),
    % then
    ?assertEqual(Expected, Csv).

json_test() ->
    % given
    Expected = [[<<"key">>, <<"val">>],
                [<<"1">>, <<"{\"type\": \"Point\",\"coordinates\": [102.0, 0.5]}">>]],
    CsvBin = <<"key,val\n1,\"{\"\"type\"\": \"\"Point\"\",\"\"coordinates\"\": [102.0, 0.5]}\"\n">>,
    % when
    {Csv, _} = csv:decode(CsvBin, csv:new()),
    % then
    ?assertEqual(Expected, Csv).
