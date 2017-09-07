# iso_erlang [![Hex.pm](https://img.shields.io/hexpm/v/iso_erlang.svg)](https://hex.pm/packages/iso_erlang)
This erlang package allows for fast case-insensitive validation and conversion between ISO Alpha-2, Alpha-3, and Numeric Country Codes.
It is a partial implementation of ISO conversion and validation. 

It currently covers: 
 * Language: ISO 639 validation and conversion 
 * Country: ISO 3166 validation and conversion

# Usage:
To use iso_erlang in your application, you need to define it as a rebar dependency or include it in Erlang's path in another way.
The hex.pm package can be found [here](https://hex.pm/packages/iso_erlang).
# Example uses:
## Validation
### ISO Alpha-2
```
1> iso_erlang:is_country_alpha_2(<<"US">>).
true
2> iso_erlang:is_country_alpha_2("US").
true
3> iso_erlang:is_country_alpha_2(<<"Us">>).
true
4> iso_erlang:is_country_alpha_2("us").
true
5> iso_erlang:is_language_alpha_2(<<"EN">>).
true
6> iso_erlang:is_language_alpha_2("eN").
true
7> iso_erlang:is_language_alpha_2(<<"En">>).
true
8> iso_erlang:is_language_alpha_2("en").
true
```
### ISO Alpha-3
```
1> iso_erlang:is_country_alpha_3(<<"USA">>).
true
2> iso_erlang:is_country_alpha_3("USA").
true
3> iso_erlang:is_country_alpha_3(<<"Usa">>).
true
4> iso_erlang:is_country_alpha_3("usa").
true
5> iso_erlang:is_language_alpha_3(<<"ENG">>).
true
6> iso_erlang:is_language_alpha_3("ENG").
true
7> iso_erlang:is_language_alpha_3(<<"Eng">>).
true
8> iso_erlang:is_language_alpha_3("eng").
true
```
### Country (Checks for both ISO Alpha-2 and ISO Alpha-3)
```
1> iso_erlang:is_country(<<"US">>).
true
2> iso_erlang:is_country("USA").
true
3> iso_erlang:is_country(<<"Usa">>).
true
4> iso_erlang:is_country("us").
true
```
### Language (Checks for both ISO Alpha-2 and ISO Alpha-3)
```
1> iso_erlang:is_language(<<"En">>).
true
2> iso_erlang:is_language("Eng").
true
3> iso_erlang:is_language(<<"Eng">>).
true
4> iso_erlang:is_language("en").
true
```
### Language normalization (Checks for both ISO Alpha-2 and ISO Alpha-3)
This function assesses if the given input can be converted into a valid ISO language
```
1> iso_erlang:is_convertible_language(<<"zh-hans">>).
true
2> iso_erlang:is_convertible_language("pt-br").
true
3> iso_erlang:is_convertible_language(<<"es-la">>).
true
4> iso_erlang:is_convertible_language("zh-hant").
true
```
## Conversion
### Country name
```
1> iso_erlang:to_country_name("US").
<<"United States of America">>
2> iso_erlang:to_country_name(<<"USA">>).
<<"United States of America">>
```
### Language name
```
1> iso_erlang:to_language_name("EN").
<<"English">>
```

### United Nations numerical code M49 for countries
```
1> iso_erlang:country_to_numerical_code("US").
840
2> iso_erlang:country_to_numerical_code(<<"USA">>).
840
```
### ISO Alpha-2
```
1> iso_erlang:to_country_alpha_2("USA").
<<"US">>
2> iso_erlang:to_country_alpha_2(<<"USA">>).
<<"US">>
3> iso_erlang:to_language_alpha_2("ENG").
<<"en">>
4> iso_erlang:to_language_alpha_2(<<"ENG">>).
<<"en">>
```
### ISO Alpha-3
```
1> iso_erlang:to_country_alpha_3("US").
<<"USA">>
2> iso_erlang:to_country_alpha_3(<<"US">>).
<<"USA">>
3> iso_erlang:to_language_alpha_3("EN").
<<"eng">>
3> iso_erlang:to_language_alpha_3(<<"EN">>).
<<"eng">>
```
