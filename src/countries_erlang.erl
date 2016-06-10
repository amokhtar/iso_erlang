-module(countries_erlang).

%% export private functions during tests.
-ifdef(TEST).
-compile(export_all).
-endif.

%% API
-export([
    alpha_2_to_name/1,
    alpha_3_to_name/1,
    alpha_2_to_numerical_code/1,
    alpha_3_to_numerical_code/1,
    is_alpha_2/1,
    is_alpha_3/1,
    is_country/1,
    to_alpha_2/1,
    to_alpha_3/1,
    to_country_name/1,
    to_numerical_code/1
]).

%% @doc Converts two letter country codes (ISO alpha-2) and the three letter country codes (ISO alpha-3)
%%      to United Nations numerical code M49 for countries.
%% @end
to_numerical_code(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:to_numerical_code_upper(normalize(Country)).

%% @doc Converts two letter country codes (ISO alpha-2) and the three letter country codes (ISO alpha-3)
%%      to Country names.
%% @end
to_country_name(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:to_country_name_upper(normalize(Country)).

%% @doc Checks if the given bitstring is a valid two letter country code (ISO alpha-2) OR
%%      a valid three letter country code (ISO alpha-3).
%% @end
is_country(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_validator:is_country_upper(normalize(Country)).

%% @doc Checks if the given bitstring is a valid three letter country code (ISO alpha-3).
%% @end
is_alpha_3(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_validator:is_alpha_3_upper(normalize(Country)).

%% @doc Checks if the given bitstring is a valid two letter country code (ISO alpha-2).
%% @end
is_alpha_2(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_validator:is_alpha_2_upper(normalize(Country)).

%% @doc Converts two letter country code (ISO alpha-2) to the equivalent three letter country code (ISO alpha-3)
%% @end
to_alpha_3(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:to_alpha_3_upper(normalize(Country)).

%% @doc Converts three letter country code (ISO alpha-3) to the equivalent two letter country code (ISO alpha-2)
%% @end
to_alpha_2(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:to_alpha_2_upper(normalize(Country)).

%% @doc Converts two letter country code (ISO alpha-2) to the equivalent country name
%% @end
alpha_2_to_name(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_2_to_name_upper(normalize(Country)).

%% @doc Converts three letter country code (ISO alpha-3) to the equivalent country name
%% @end
alpha_3_to_name(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_3_to_name_upper(normalize(Country)).

%% @doc Converts three letter country code (ISO alpha-3) to the equivalent United Nations numerical code M49 for countries.
%% @end
alpha_3_to_numerical_code(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_3_to_numerical_code_upper(normalize(Country)).

%% @doc Converts two letter country code (ISO alpha-2) to the equivalent United Nations numerical code M49 for countries.
%% @end
alpha_2_to_numerical_code(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_2_to_numerical_code_upper(normalize(Country)).

normalize(String) when is_list(String) ->
    list_to_bitstring(string:to_upper(String));
normalize(Bitstring) when is_bitstring(Bitstring) ->
    <<<<(string:to_upper(X))>> || <<X>> <= Bitstring>>.
