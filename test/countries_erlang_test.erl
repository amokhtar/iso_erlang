-module(countries_erlang_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

is_alpha_2_true_test() ->
    ?assertEqual(true, countries_erlang:is_alpha_2(<<"EG">>)),
    ?assertEqual(true, countries_erlang:is_alpha_2(<<"US">>)),
    ?assertEqual(true, countries_erlang:is_alpha_2(<<"us">>)),
    ?assertEqual(true, countries_erlang:is_alpha_2("EG")),
    ?assertEqual(true, countries_erlang:is_alpha_2("eg")),
    ?assertEqual(true, countries_erlang:is_alpha_2("US")).

is_alpha_2_false_test() ->
    ?assertEqual(false, countries_erlang:is_alpha_2(<<"EGG">>)),
    ?assertEqual(false, countries_erlang:is_alpha_2(<<"AA">>)),
    ?assertEqual(false, countries_erlang:is_alpha_2("egss")),
    ?assertEqual(false, countries_erlang:is_alpha_2("USA")).

is_alpha_3_true_test() ->
    ?assertEqual(true, countries_erlang:is_alpha_3(<<"EGY">>)),
    ?assertEqual(true, countries_erlang:is_alpha_3(<<"USA">>)),
    ?assertEqual(true, countries_erlang:is_alpha_3(<<"usa">>)),
    ?assertEqual(true, countries_erlang:is_alpha_3("EGY")),
    ?assertEqual(true, countries_erlang:is_alpha_3("egy")),
    ?assertEqual(true, countries_erlang:is_alpha_3("USA")).

is_alpha_3_false_test() ->
    ?assertEqual(false, countries_erlang:is_alpha_3(<<"EG">>)),
    ?assertEqual(false, countries_erlang:is_alpha_3(<<"AAA">>)),
    ?assertEqual(false, countries_erlang:is_alpha_3("usdsda")),
    ?assertEqual(false, countries_erlang:is_alpha_3("US")).

is_country_true_test() ->
    ?assertEqual(true, countries_erlang:is_country(<<"EGY">>)),
    ?assertEqual(true, countries_erlang:is_country(<<"USA">>)),
    ?assertEqual(true, countries_erlang:is_country(<<"EgY">>)),
    ?assertEqual(true, countries_erlang:is_country(<<"UsA">>)),
    ?assertEqual(true, countries_erlang:is_country("EGY")),
    ?assertEqual(true, countries_erlang:is_country("USA")),
    ?assertEqual(true, countries_erlang:is_country(<<"EG">>)),
    ?assertEqual(true, countries_erlang:is_country(<<"US">>)),
    ?assertEqual(true, countries_erlang:is_country("EG")),
    ?assertEqual(true, countries_erlang:is_country("US")).

is_country_false_test() ->
    ?assertEqual(false, countries_erlang:is_country(<<"EGG">>)),
    ?assertEqual(false, countries_erlang:is_country(<<"AA">>)),
    ?assertEqual(false, countries_erlang:is_country("sdfg")),
    ?assertEqual(false, countries_erlang:is_country("SASA")),
    ?assertEqual(false, countries_erlang:is_country(<<"EGd">>)),
    ?assertEqual(false, countries_erlang:is_country(<<"AAA">>)),
    ?assertEqual(false, countries_erlang:is_country("sdd")),
    ?assertEqual(false, countries_erlang:is_country("USs")).

to_alpha_2_correct_test() ->
    ?assertEqual(<<"EG">>, countries_erlang:to_alpha_2(<<"EGY">>)),
    ?assertEqual(<<"US">>, countries_erlang:to_alpha_2(<<"USA">>)),
    ?assertEqual(<<"EG">>, countries_erlang:to_alpha_2("EGY")),
    ?assertEqual(<<"US">>, countries_erlang:to_alpha_2("USA")).

to_alpha_2_already_correct_test() ->
    ?assertEqual(<<"EG">>, countries_erlang:to_alpha_2(<<"EG">>)),
    ?assertEqual(<<"US">>, countries_erlang:to_alpha_2(<<"US">>)),
    ?assertEqual(<<"EG">>, countries_erlang:to_alpha_2("EG")),
    ?assertEqual(<<"US">>, countries_erlang:to_alpha_2("US")).

to_alpha_2_incorrect_test() ->
    ?assertError(_, countries_erlang:to_alpha_2(<<"EGR">>)),
    ?assertError(_, countries_erlang:to_alpha_2(<<"AAA">>)),
    ?assertError(_, countries_erlang:to_alpha_2("EGs")),
    ?assertError(_, countries_erlang:to_alpha_2("AAA")).

to_alpha_3_correct_test() ->
    ?assertEqual(<<"EGY">>, countries_erlang:to_alpha_3(<<"EG">>)),
    ?assertEqual(<<"USA">>, countries_erlang:to_alpha_3(<<"US">>)),
    ?assertEqual(<<"EGY">>, countries_erlang:to_alpha_3("EG")),
    ?assertEqual(<<"USA">>, countries_erlang:to_alpha_3("US")).

to_alpha_3_already_correct_test() ->
    ?assertEqual(<<"EGY">>, countries_erlang:to_alpha_3(<<"EGY">>)),
    ?assertEqual(<<"USA">>, countries_erlang:to_alpha_3(<<"USA">>)),
    ?assertEqual(<<"EGY">>, countries_erlang:to_alpha_3("EGY")),
    ?assertEqual(<<"USA">>, countries_erlang:to_alpha_3("USA")).

to_alpha_3_incorrect_test() ->
    ?assertError(_, countries_erlang:to_alpha_3(<<"EGR">>)),
    ?assertError(_, countries_erlang:to_alpha_3(<<"AAA">>)),
    ?assertError(_, countries_erlang:to_alpha_3("EGs")),
    ?assertError(_, countries_erlang:to_alpha_3("AAA")).

alpha_2_to_name_correct_test() ->
    ?assertEqual(<<"Egypt">>, countries_erlang:alpha_2_to_name(<<"EG">>)),
    ?assertEqual(<<"United States of America">>, countries_erlang:alpha_2_to_name(<<"US">>)),
    ?assertEqual(<<"Egypt">>, countries_erlang:alpha_2_to_name("EG")),
    ?assertEqual(<<"United States of America">>, countries_erlang:alpha_2_to_name("US")).

alpha_2_to_name_incorrect_test() ->
    ?assertError(_, countries_erlang:alpha_2_to_name(<<"EGY">>)),
    ?assertError(_, countries_erlang:alpha_2_to_name(<<"USA">>)),
    ?assertError(_, countries_erlang:alpha_2_to_name("ABC")),
    ?assertError(_, countries_erlang:alpha_2_to_name("AAA")).

alpha_3_to_name_correct_test() ->
    ?assertEqual(<<"Egypt">>, countries_erlang:alpha_3_to_name(<<"EGY">>)),
    ?assertEqual(<<"United States of America">>, countries_erlang:alpha_3_to_name(<<"USA">>)),
    ?assertEqual(<<"Egypt">>, countries_erlang:alpha_3_to_name("EGY")),
    ?assertEqual(<<"United States of America">>, countries_erlang:alpha_3_to_name("USA")).

alpha_3_to_name_incorrect_test() ->
    ?assertError(_, countries_erlang:alpha_3_to_name(<<"EG">>)),
    ?assertError(_, countries_erlang:alpha_3_to_name(<<"US">>)),
    ?assertError(_, countries_erlang:alpha_3_to_name("ABA")),
    ?assertError(_, countries_erlang:alpha_3_to_name("AA")).

to_country_name_correct_test() ->
    ?assertEqual(<<"Egypt">>, countries_erlang:to_country_name(<<"EGY">>)),
    ?assertEqual(<<"United States of America">>, countries_erlang:to_country_name(<<"USA">>)),
    ?assertEqual(<<"Egypt">>, countries_erlang:to_country_name("EGY")),
    ?assertEqual(<<"United States of America">>, countries_erlang:to_country_name("USA")),
    ?assertEqual(<<"Egypt">>, countries_erlang:to_country_name(<<"EG">>)),
    ?assertEqual(<<"United States of America">>, countries_erlang:to_country_name(<<"US">>)),
    ?assertEqual(<<"Egypt">>, countries_erlang:to_country_name("EG")),
    ?assertEqual(<<"United States of America">>, countries_erlang:to_country_name("US")).

to_country_name_incorrect_test() ->
    ?assertError(_, countries_erlang:to_country_name(<<"ABC">>)),
    ?assertError(_, countries_erlang:to_country_name(<<"AAA">>)),
    ?assertError(_, countries_erlang:to_country_name("ABC")),
    ?assertError(_, countries_erlang:to_country_name("AAA")).

alpha_2_to_numerical_code_correct_test() ->
    ?assertEqual(818, countries_erlang:alpha_2_to_numerical_code(<<"EG">>)),
    ?assertEqual(840, countries_erlang:alpha_2_to_numerical_code(<<"US">>)),
    ?assertEqual(818, countries_erlang:alpha_2_to_numerical_code("EG")),
    ?assertEqual(840, countries_erlang:alpha_2_to_numerical_code("US")).

alpha_2_to_numerical_code_incorrect_test() ->
    ?assertError(_, countries_erlang:alpha_2_to_numerical_code(<<"EGY">>)),
    ?assertError(_, countries_erlang:alpha_2_to_numerical_code(<<"USA">>)),
    ?assertError(_, countries_erlang:alpha_2_to_numerical_code("EGY")),
    ?assertError(_, countries_erlang:alpha_2_to_numerical_code("USA")).

alpha_3_to_numerical_code_correct_test() ->
    ?assertEqual(818, countries_erlang:alpha_3_to_numerical_code(<<"EGY">>)),
    ?assertEqual(840, countries_erlang:alpha_3_to_numerical_code(<<"USA">>)),
    ?assertEqual(818, countries_erlang:alpha_3_to_numerical_code("EGY")),
    ?assertEqual(840, countries_erlang:alpha_3_to_numerical_code("USA")).

alpha_3_to_numerical_code_incorrect_test() ->
    ?assertError(_, countries_erlang:alpha_3_to_numerical_code(<<"EG">>)),
    ?assertError(_, countries_erlang:alpha_3_to_numerical_code(<<"US">>)),
    ?assertError(_, countries_erlang:alpha_3_to_numerical_code("EG")),
    ?assertError(_, countries_erlang:alpha_3_to_numerical_code("US")).

to_numerical_code_correct_test() ->
    ?assertEqual(818, countries_erlang:to_numerical_code(<<"EGY">>)),
    ?assertEqual(840, countries_erlang:to_numerical_code(<<"USA">>)),
    ?assertEqual(818, countries_erlang:to_numerical_code("EGY")),
    ?assertEqual(840, countries_erlang:to_numerical_code("USA")),
    ?assertEqual(818, countries_erlang:to_numerical_code(<<"EG">>)),
    ?assertEqual(840, countries_erlang:to_numerical_code(<<"US">>)),
    ?assertEqual(818, countries_erlang:to_numerical_code("EG")),
    ?assertEqual(840, countries_erlang:to_numerical_code("US")).

to_numerical_code_incorrect_test() ->
    ?assertError(_, countries_erlang:to_numerical_code(<<"EsdG">>)),
    ?assertError(_, countries_erlang:to_numerical_code(<<"UsS">>)),
    ?assertError(_, countries_erlang:to_numerical_code("EGs")),
    ?assertError(_, countries_erlang:to_numerical_code("sUS")).

normalize_test() ->
    ?assertEqual(<<"HELLO">>, countries_erlang:normalize(<<"Hello">>)),
    ?assertEqual(<<"HELLO">>, countries_erlang:normalize(<<"HeLlO">>)),
    ?assertEqual(<<"HELLO">>, countries_erlang:normalize("hello")),
    ?assertEqual(<<"HELLO">>, countries_erlang:normalize("HeLlO")).

