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
    get_alpha_2_country_list/0,
    get_alpha_3_country_list/0,
    get_country_name_list/0,
    is_alpha_2/1,
    is_alpha_3/1,
    is_country/1,
    to_alpha_2/1,
    to_alpha_3/1,
    to_country_name/1,
    to_numerical_code/1
]).

%% @doc Returns a list of all countries in ISO alpha-2 format
%% @end
-spec get_alpha_2_country_list() -> [bitstring()].
get_alpha_2_country_list() ->
    countries_erlang_lists:get_alpha_2_list().

%% @doc Returns a list of all countries in ISO alpha-3 format
%% @end
-spec get_alpha_3_country_list() -> [bitstring()].
get_alpha_3_country_list() ->
    countries_erlang_lists:get_alpha_3_list().

%% @doc Returns a list of all country names
%% @end
-spec get_country_name_list() -> [bitstring()].
get_country_name_list() ->
    countries_erlang_lists:get_country_name_list().

-spec to_numerical_code(bitstring() | string()) -> integer().
to_numerical_code(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:to_numerical_code_upper(normalize(Country)).

%% @doc Converts two letter country codes (ISO alpha-2) and the three letter country codes (ISO alpha-3)
%%      to Country names.
%% @end
-spec to_country_name(bitstring() | string()) -> bitstring().
to_country_name(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:to_country_name_upper(normalize(Country)).

%% @doc Checks if the given bitstring is a valid two letter country code (ISO alpha-2) OR
%%      a valid three letter country code (ISO alpha-3).
%% @end
-spec is_country(bitstring() | string()) -> boolean().
is_country(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_validator:is_country_upper(normalize(Country)).

%% @doc Checks if the given bitstring is a valid three letter country code (ISO alpha-3).
%% @end
-spec is_alpha_3(bitstring() | string()) -> boolean().
is_alpha_3(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_validator:is_alpha_3_upper(normalize(Country)).

%% @doc Checks if the given bitstring is a valid two letter country code (ISO alpha-2).
%% @end
-spec is_alpha_2(bitstring() | string()) -> boolean().
is_alpha_2(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_validator:is_alpha_2_upper(normalize(Country)).

%% @doc Converts two letter country code (ISO alpha-2) to the equivalent three letter country code (ISO alpha-3)
%% @end
-spec to_alpha_3(bitstring() | string()) -> bitstring().
to_alpha_3(Country) when is_list(Country) orelse is_bitstring(Country) ->
    NormalizedCountry = normalize(Country),
    case is_alpha_3(NormalizedCountry) of
        true -> NormalizedCountry;
        false -> countries_erlang_converter:to_alpha_3_upper(NormalizedCountry)
    end.

%% @doc Converts three letter country code (ISO alpha-3) to the equivalent two letter country code (ISO alpha-2)
%% @end
-spec to_alpha_2(bitstring() | string()) -> bitstring().
to_alpha_2(Country) when is_list(Country) orelse is_bitstring(Country) ->
    NormalizedCountry = normalize(Country),
    case is_alpha_2(NormalizedCountry) of
        true -> NormalizedCountry;
        false -> countries_erlang_converter:to_alpha_2_upper(NormalizedCountry)
    end.

%% @doc Converts two letter country code (ISO alpha-2) to the equivalent country name
%% @end
-spec alpha_2_to_name(bitstring() | string()) -> bitstring().
alpha_2_to_name(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_2_to_name_upper(normalize(Country)).

%% @doc Converts three letter country code (ISO alpha-3) to the equivalent country name
%% @end
-spec alpha_3_to_name(bitstring() | string()) -> bitstring().
alpha_3_to_name(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_3_to_name_upper(normalize(Country)).

%% @doc Converts three letter country code (ISO alpha-3) to the equivalent United Nations numerical code M49 for countries.
%% @end
-spec alpha_3_to_numerical_code(bitstring() | string()) -> integer().
alpha_3_to_numerical_code(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_3_to_numerical_code_upper(normalize(Country)).

%% @doc Converts two letter country code (ISO alpha-2) to the equivalent United Nations numerical code M49 for countries.
%% @end
-spec alpha_2_to_numerical_code(bitstring() | string()) -> integer().
alpha_2_to_numerical_code(Country) when is_list(Country) orelse is_bitstring(Country) -> countries_erlang_converter:alpha_2_to_numerical_code_upper(normalize(Country)).

normalize(String) when is_list(String) ->
    list_to_bitstring(string:to_upper(String));
normalize(Bitstring) when is_bitstring(Bitstring) ->
    <<<<(string:to_upper(X))>> || <<X>> <= Bitstring>>.
