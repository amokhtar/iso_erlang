-module(iso_erlang_language_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

is_alpha_2_true_test() ->
    ?assertEqual(true, iso_erlang:is_language_alpha_2(<<"FR">>)),
    ?assertEqual(true, iso_erlang:is_language_alpha_2(<<"EN">>)),
    ?assertEqual(true, iso_erlang:is_language_alpha_2(<<"en">>)),
    ?assertEqual(true, iso_erlang:is_language_alpha_2("En")),
    ?assertEqual(true, iso_erlang:is_language_alpha_2("fR")).

is_alpha_2_false_test() ->
    ?assertEqual(false, iso_erlang:is_language_alpha_2(<<"ENG">>)),
    ?assertEqual(false, iso_erlang:is_language_alpha_2(<<"xx">>)),
    ?assertEqual(false, iso_erlang:is_language_alpha_2("adfs")),
    ?assertEqual(false, iso_erlang:is_language_alpha_2("a")).

is_alpha_3_true_test() ->
    ?assertEqual(true, iso_erlang:is_language_alpha_3(<<"ENG">>)),
    ?assertEqual(true, iso_erlang:is_language_alpha_3(<<"DZO">>)),
    ?assertEqual(true, iso_erlang:is_language_alpha_3(<<"FAS">>)),
    ?assertEqual(true, iso_erlang:is_language_alpha_3("FRE")),
    ?assertEqual(true, iso_erlang:is_language_alpha_3("fra")),
    ?assertEqual(true, iso_erlang:is_language_alpha_3("ChV")).

is_alpha_3_false_test() ->
    ?assertEqual(false, iso_erlang:is_language_alpha_3(<<"EN">>)),
    ?assertEqual(false, iso_erlang:is_language_alpha_3(<<"en">>)),
    ?assertEqual(false, iso_erlang:is_language_alpha_3("asdfsdf")),
    ?assertEqual(false, iso_erlang:is_language_alpha_3("w")).

is_language_true_test() ->
    ?assertEqual(true, iso_erlang:is_language(<<"ENG">>)),
    ?assertEqual(true, iso_erlang:is_language(<<"DZO">>)),
    ?assertEqual(true, iso_erlang:is_language(<<"FAS">>)),
    ?assertEqual(true, iso_erlang:is_language("FRE")),
    ?assertEqual(true, iso_erlang:is_language("fra")),
    ?assertEqual(true, iso_erlang:is_language("ChV")),
    ?assertEqual(true, iso_erlang:is_language(<<"FR">>)),
    ?assertEqual(true, iso_erlang:is_language(<<"EN">>)),
    ?assertEqual(true, iso_erlang:is_language(<<"en">>)),
    ?assertEqual(true, iso_erlang:is_language("En")),
    ?assertEqual(true, iso_erlang:is_language("fR")).

is_language_false_test() ->
    ?assertEqual(false, iso_erlang:is_language(<<"EGGEGG">>)),
    ?assertEqual(false, iso_erlang:is_language(<<"">>)),
    ?assertEqual(false, iso_erlang:is_language("UadfsSs")).

to_alpha_2_correct_test() ->
    ?assertEqual(<<"EN">>, iso_erlang:to_language_alpha_2(<<"ENG">>)),
    ?assertEqual(<<"FR">>, iso_erlang:to_language_alpha_2(<<"FRA">>)),
    ?assertEqual(<<"EN">>, iso_erlang:to_language_alpha_2("ENG")),
    ?assertEqual(<<"FR">>, iso_erlang:to_language_alpha_2("FRE")).

to_alpha_2_already_correct_test() ->
    ?assertEqual(<<"EN">>, iso_erlang:to_language_alpha_2(<<"EN">>)),
    ?assertEqual(<<"FR">>, iso_erlang:to_language_alpha_2(<<"FR">>)),
    ?assertEqual(<<"EN">>, iso_erlang:to_language_alpha_2("EN")),
    ?assertEqual(<<"FR">>, iso_erlang:to_language_alpha_2("FR")).

to_alpha_2_incorrect_test() ->
    ?assertError(_, iso_erlang:to_language_alpha_2(<<"EGR">>)),
    ?assertError(_, iso_erlang:to_language_alpha_2(<<"AAA">>)),
    ?assertError(_, iso_erlang:to_language_alpha_2("EGs")),
    ?assertError(_, iso_erlang:to_language_alpha_2("AAA")).

to_alpha_3_correct_test() ->
    ?assertEqual(<<"ENG">>, iso_erlang:to_language_alpha_3(<<"EN">>)),
    ?assertEqual(<<"FRA">>, iso_erlang:to_language_alpha_3(<<"FR">>)),
    ?assertEqual(<<"ENG">>, iso_erlang:to_language_alpha_3("EN")),
    ?assertEqual(<<"FRA">>, iso_erlang:to_language_alpha_3("FR")).

to_alpha_3_already_correct_test() ->
    ?assertEqual(<<"ENG">>, iso_erlang:to_language_alpha_3(<<"ENG">>)),
    ?assertEqual(<<"FRA">>, iso_erlang:to_language_alpha_3(<<"FRA">>)),
    ?assertEqual(<<"ENG">>, iso_erlang:to_language_alpha_3("ENG")),
    ?assertEqual(<<"FRA">>, iso_erlang:to_language_alpha_3("FRA")).

to_alpha_3_incorrect_test() ->
    ?assertError(_, iso_erlang:to_language_alpha_3(<<"EGR">>)),
    ?assertError(_, iso_erlang:to_language_alpha_3(<<"AAA">>)),
    ?assertError(_, iso_erlang:to_language_alpha_3("EGs")),
    ?assertError(_, iso_erlang:to_language_alpha_3("AAA")).

alpha_2_to_name_correct_test() ->
    ?assertEqual(<<"English">>, iso_erlang:language_alpha_2_to_name(<<"EN">>)),
    ?assertEqual(<<"Arabic">>, iso_erlang:language_alpha_2_to_name(<<"AR">>)),
    ?assertEqual(<<"English">>, iso_erlang:language_alpha_2_to_name("EN")),
    ?assertEqual(<<"Arabic">>, iso_erlang:language_alpha_2_to_name("AR")).

alpha_2_to_name_incorrect_test() ->
    ?assertError(_, iso_erlang:language_alpha_2_to_name(<<"EG">>)),
    ?assertError(_, iso_erlang:language_alpha_2_to_name(<<"USA">>)),
    ?assertError(_, iso_erlang:language_alpha_2_to_name("ABC")),
    ?assertError(_, iso_erlang:language_alpha_2_to_name("AAA")).

alpha_3_to_name_correct_test() ->
    ?assertEqual(<<"Egyptian (Ancient)">>, iso_erlang:language_alpha_3_to_name(<<"EGY">>)),
    ?assertEqual(<<"English">>, iso_erlang:language_alpha_3_to_name(<<"ENG">>)),
    ?assertEqual(<<"Egyptian (Ancient)">>, iso_erlang:language_alpha_3_to_name("EGY")),
    ?assertEqual(<<"English">>, iso_erlang:language_alpha_3_to_name("ENG")).

alpha_3_to_name_incorrect_test() ->
    ?assertError(_, iso_erlang:language_alpha_3_to_name(<<"EG">>)),
    ?assertError(_, iso_erlang:language_alpha_3_to_name(<<"US">>)),
    ?assertError(_, iso_erlang:language_alpha_3_to_name("ABA")),
    ?assertError(_, iso_erlang:language_alpha_3_to_name("AA")).

to_language_name_correct_test() ->
    ?assertEqual(<<"Egyptian (Ancient)">>, iso_erlang:to_language_name(<<"EGY">>)),
    ?assertEqual(<<"English">>, iso_erlang:to_language_name(<<"ENG">>)),
    ?assertEqual(<<"Egyptian (Ancient)">>, iso_erlang:to_language_name("EGY")),
    ?assertEqual(<<"English">>, iso_erlang:to_language_name("ENG")),
    ?assertEqual(<<"English">>, iso_erlang:to_language_name(<<"EN">>)),
    ?assertEqual(<<"Arabic">>, iso_erlang:to_language_name(<<"AR">>)),
    ?assertEqual(<<"English">>, iso_erlang:to_language_name("EN")),
    ?assertEqual(<<"Arabic">>, iso_erlang:to_language_name("AR")).

to_language_name_incorrect_test() ->
    ?assertError(_, iso_erlang:to_language_name(<<"ABC">>)),
    ?assertError(_, iso_erlang:to_language_name(<<"AAA">>)),
    ?assertError(_, iso_erlang:to_language_name("ABC")),
    ?assertError(_, iso_erlang:to_language_name("AAA")).

normalize_test() ->
    ?assertEqual(<<"HELLO">>, iso_erlang:normalize(<<"Hello">>)),
    ?assertEqual(<<"HELLO">>, iso_erlang:normalize(<<"HeLlO">>)),
    ?assertEqual(<<"HELLO">>, iso_erlang:normalize("hello")),
    ?assertEqual(<<"HELLO">>, iso_erlang:normalize("HeLlO")).

get_language_lists_test() ->
    ?assertEqual(184, length(iso_erlang:get_alpha_2_language_list())),
    ?assertEqual(488, length(iso_erlang:get_alpha_3_language_list())),
    ?assertEqual(565, length(iso_erlang:get_language_name_list())).