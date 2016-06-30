-module(iso_erlang_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

to_upper_test() ->
    ?assertEqual(<<"HELLO">>, iso_erlang:to_upper(<<"Hello">>)),
    ?assertEqual(<<"HELLO">>, iso_erlang:to_upper(<<"HeLlO">>)),
    ?assertEqual(<<"HELLO">>, iso_erlang:to_upper("hello")),
    ?assertEqual(<<"HELLO">>, iso_erlang:to_upper("HeLlO")).

to_lower_test() ->
    ?assertEqual(<<"hello">>, iso_erlang:to_lower(<<"Hello">>)),
    ?assertEqual(<<"hello">>, iso_erlang:to_lower(<<"HeLlO">>)),
    ?assertEqual(<<"hello">>, iso_erlang:to_lower("hello")),
    ?assertEqual(<<"hello">>, iso_erlang:to_lower("HeLlO")).