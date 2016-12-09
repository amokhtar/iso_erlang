-module(iso_erlang_country_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

is_alpha_2_true_test() ->
    ?assertEqual(true, iso_erlang:is_country_alpha_2(<<"EG">>)),
    ?assertEqual(true, iso_erlang:is_country_alpha_2(<<"US">>)),
    ?assertEqual(true, iso_erlang:is_country_alpha_2(<<"us">>)),
    ?assertEqual(true, iso_erlang:is_country_alpha_2("EG")),
    ?assertEqual(true, iso_erlang:is_country_alpha_2("eg")),
    ?assertEqual(true, iso_erlang:is_country_alpha_2("US")).

is_alpha_2_false_test() ->
    ?assertEqual(false, iso_erlang:is_country_alpha_2(<<"EGG">>)),
    ?assertEqual(false, iso_erlang:is_country_alpha_2(<<"AA">>)),
    ?assertEqual(false, iso_erlang:is_country_alpha_2("egss")),
    ?assertEqual(false, iso_erlang:is_country_alpha_2("USA")).

is_alpha_3_true_test() ->
    ?assertEqual(true, iso_erlang:is_country_alpha_3(<<"EGY">>)),
    ?assertEqual(true, iso_erlang:is_country_alpha_3(<<"USA">>)),
    ?assertEqual(true, iso_erlang:is_country_alpha_3(<<"usa">>)),
    ?assertEqual(true, iso_erlang:is_country_alpha_3("EGY")),
    ?assertEqual(true, iso_erlang:is_country_alpha_3("egy")),
    ?assertEqual(true, iso_erlang:is_country_alpha_3("USA")).

is_alpha_3_false_test() ->
    ?assertEqual(false, iso_erlang:is_country_alpha_3(<<"EG">>)),
    ?assertEqual(false, iso_erlang:is_country_alpha_3(<<"AAA">>)),
    ?assertEqual(false, iso_erlang:is_country_alpha_3("usdsda")),
    ?assertEqual(false, iso_erlang:is_country_alpha_3("US")).

is_country_true_test() ->
    ?assertEqual(true, iso_erlang:is_country(<<"EGY">>)),
    ?assertEqual(true, iso_erlang:is_country(<<"USA">>)),
    ?assertEqual(true, iso_erlang:is_country(<<"EgY">>)),
    ?assertEqual(true, iso_erlang:is_country(<<"UsA">>)),
    ?assertEqual(true, iso_erlang:is_country("EGY")),
    ?assertEqual(true, iso_erlang:is_country("USA")),
    ?assertEqual(true, iso_erlang:is_country(<<"EG">>)),
    ?assertEqual(true, iso_erlang:is_country(<<"US">>)),
    ?assertEqual(true, iso_erlang:is_country("EG")),
    ?assertEqual(true, iso_erlang:is_country("US")).

is_country_false_test() ->
    ?assertEqual(false, iso_erlang:is_country(<<"EGG">>)),
    ?assertEqual(false, iso_erlang:is_country(<<"AA">>)),
    ?assertEqual(false, iso_erlang:is_country("sdfg")),
    ?assertEqual(false, iso_erlang:is_country("SASA")),
    ?assertEqual(false, iso_erlang:is_country(<<"EGd">>)),
    ?assertEqual(false, iso_erlang:is_country(<<"AAA">>)),
    ?assertEqual(false, iso_erlang:is_country("sdd")),
    ?assertEqual(false, iso_erlang:is_country("USs")).

to_alpha_2_correct_test() ->
    ?assertEqual(<<"EG">>, iso_erlang:to_country_alpha_2(<<"EGY">>)),
    ?assertEqual(<<"US">>, iso_erlang:to_country_alpha_2(<<"USA">>)),
    ?assertEqual(<<"EG">>, iso_erlang:to_country_alpha_2("EGY")),
    ?assertEqual(<<"US">>, iso_erlang:to_country_alpha_2("USA")).

to_alpha_2_already_correct_test() ->
    ?assertEqual(<<"EG">>, iso_erlang:to_country_alpha_2(<<"EG">>)),
    ?assertEqual(<<"US">>, iso_erlang:to_country_alpha_2(<<"US">>)),
    ?assertEqual(<<"EG">>, iso_erlang:to_country_alpha_2("EG")),
    ?assertEqual(<<"US">>, iso_erlang:to_country_alpha_2("US")).

to_alpha_2_incorrect_test() ->
    ?assertError(_, iso_erlang:to_country_alpha_2(<<"EGR">>)),
    ?assertError(_, iso_erlang:to_country_alpha_2(<<"AAA">>)),
    ?assertError(_, iso_erlang:to_country_alpha_2("EGs")),
    ?assertError(_, iso_erlang:to_country_alpha_2("AAA")).

to_alpha_3_correct_test() ->
    ?assertEqual(<<"EGY">>, iso_erlang:to_country_alpha_3(<<"EG">>)),
    ?assertEqual(<<"USA">>, iso_erlang:to_country_alpha_3(<<"US">>)),
    ?assertEqual(<<"EGY">>, iso_erlang:to_country_alpha_3("EG")),
    ?assertEqual(<<"USA">>, iso_erlang:to_country_alpha_3("US")).

to_alpha_3_already_correct_test() ->
    ?assertEqual(<<"EGY">>, iso_erlang:to_country_alpha_3(<<"EGY">>)),
    ?assertEqual(<<"USA">>, iso_erlang:to_country_alpha_3(<<"USA">>)),
    ?assertEqual(<<"EGY">>, iso_erlang:to_country_alpha_3("EGY")),
    ?assertEqual(<<"USA">>, iso_erlang:to_country_alpha_3("USA")).

to_alpha_3_incorrect_test() ->
    ?assertError(_, iso_erlang:to_country_alpha_3(<<"EGR">>)),
    ?assertError(_, iso_erlang:to_country_alpha_3(<<"AAA">>)),
    ?assertError(_, iso_erlang:to_country_alpha_3("EGs")),
    ?assertError(_, iso_erlang:to_country_alpha_3("AAA")).

alpha_2_to_name_correct_test() ->
    ?assertEqual(<<"Egypt">>, iso_erlang:country_alpha_2_to_name(<<"EG">>)),
    ?assertEqual(<<"United States of America">>, iso_erlang:country_alpha_2_to_name(<<"US">>)),
    ?assertEqual(<<"Egypt">>, iso_erlang:country_alpha_2_to_name("EG")),
    ?assertEqual(<<"United States of America">>, iso_erlang:country_alpha_2_to_name("US")).

alpha_2_to_name_incorrect_test() ->
    ?assertError(_, iso_erlang:country_alpha_2_to_name(<<"EGY">>)),
    ?assertError(_, iso_erlang:country_alpha_2_to_name(<<"USA">>)),
    ?assertError(_, iso_erlang:country_alpha_2_to_name("ABC")),
    ?assertError(_, iso_erlang:country_alpha_2_to_name("AAA")).

alpha_3_to_name_correct_test() ->
    ?assertEqual(<<"Egypt">>, iso_erlang:country_alpha_3_to_name(<<"EGY">>)),
    ?assertEqual(<<"United States of America">>, iso_erlang:country_alpha_3_to_name(<<"USA">>)),
    ?assertEqual(<<"Egypt">>, iso_erlang:country_alpha_3_to_name("EGY")),
    ?assertEqual(<<"United States of America">>, iso_erlang:country_alpha_3_to_name("USA")).

alpha_3_to_name_incorrect_test() ->
    ?assertError(_, iso_erlang:country_alpha_3_to_name(<<"EG">>)),
    ?assertError(_, iso_erlang:country_alpha_3_to_name(<<"US">>)),
    ?assertError(_, iso_erlang:country_alpha_3_to_name("ABA")),
    ?assertError(_, iso_erlang:country_alpha_3_to_name("AA")).

to_country_name_correct_test() ->
    ?assertEqual(<<"Egypt">>, iso_erlang:to_country_name(<<"EGY">>)),
    ?assertEqual(<<"United States of America">>, iso_erlang:to_country_name(<<"USA">>)),
    ?assertEqual(<<"Egypt">>, iso_erlang:to_country_name("EGY")),
    ?assertEqual(<<"United States of America">>, iso_erlang:to_country_name("USA")),
    ?assertEqual(<<"Egypt">>, iso_erlang:to_country_name(<<"EG">>)),
    ?assertEqual(<<"United States of America">>, iso_erlang:to_country_name(<<"US">>)),
    ?assertEqual(<<"Egypt">>, iso_erlang:to_country_name("EG")),
    ?assertEqual(<<"United States of America">>, iso_erlang:to_country_name("US")).

to_country_name_incorrect_test() ->
    ?assertError(_, iso_erlang:to_country_name(<<"ABC">>)),
    ?assertError(_, iso_erlang:to_country_name(<<"AAA">>)),
    ?assertError(_, iso_erlang:to_country_name("ABC")),
    ?assertError(_, iso_erlang:to_country_name("AAA")).

alpha_2_to_numerical_code_correct_test() ->
    ?assertEqual(818, iso_erlang:country_alpha_2_to_numerical_code(<<"EG">>)),
    ?assertEqual(840, iso_erlang:country_alpha_2_to_numerical_code(<<"US">>)),
    ?assertEqual(818, iso_erlang:country_alpha_2_to_numerical_code("EG")),
    ?assertEqual(840, iso_erlang:country_alpha_2_to_numerical_code("US")).

alpha_2_to_numerical_code_incorrect_test() ->
    ?assertError(_, iso_erlang:country_alpha_2_to_numerical_code(<<"EGY">>)),
    ?assertError(_, iso_erlang:country_alpha_2_to_numerical_code(<<"USA">>)),
    ?assertError(_, iso_erlang:country_alpha_2_to_numerical_code("EGY")),
    ?assertError(_, iso_erlang:country_alpha_2_to_numerical_code("USA")).

alpha_3_to_numerical_code_correct_test() ->
    ?assertEqual(818, iso_erlang:country_alpha_3_to_numerical_code(<<"EGY">>)),
    ?assertEqual(840, iso_erlang:country_alpha_3_to_numerical_code(<<"USA">>)),
    ?assertEqual(818, iso_erlang:country_alpha_3_to_numerical_code("EGY")),
    ?assertEqual(840, iso_erlang:country_alpha_3_to_numerical_code("USA")).

alpha_3_to_numerical_code_incorrect_test() ->
    ?assertError(_, iso_erlang:country_alpha_3_to_numerical_code(<<"EG">>)),
    ?assertError(_, iso_erlang:country_alpha_3_to_numerical_code(<<"US">>)),
    ?assertError(_, iso_erlang:country_alpha_3_to_numerical_code("EG")),
    ?assertError(_, iso_erlang:country_alpha_3_to_numerical_code("US")).

to_numerical_code_correct_test() ->
    ?assertEqual(818, iso_erlang:country_to_numerical_code(<<"EGY">>)),
    ?assertEqual(840, iso_erlang:country_to_numerical_code(<<"USA">>)),
    ?assertEqual(818, iso_erlang:country_to_numerical_code("EGY")),
    ?assertEqual(840, iso_erlang:country_to_numerical_code("USA")),
    ?assertEqual(818, iso_erlang:country_to_numerical_code(<<"EG">>)),
    ?assertEqual(840, iso_erlang:country_to_numerical_code(<<"US">>)),
    ?assertEqual(818, iso_erlang:country_to_numerical_code("EG")),
    ?assertEqual(840, iso_erlang:country_to_numerical_code("US")).

to_numerical_code_incorrect_test() ->
    ?assertError(_, iso_erlang:country_to_numerical_code(<<"EsdG">>)),
    ?assertError(_, iso_erlang:country_to_numerical_code(<<"UsS">>)),
    ?assertError(_, iso_erlang:country_to_numerical_code("EGs")),
    ?assertError(_, iso_erlang:country_to_numerical_code("sUS")).

get_country_lists_test() ->
    ?assertEqual(249, length(iso_erlang:get_alpha_2_country_list())),
    ?assertEqual(249, length(iso_erlang:get_alpha_3_country_list())),
    ?assertEqual(249, length(iso_erlang:get_country_name_list())).