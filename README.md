# countries_erlang
This erlang package allows for fast case-insensitive validation and conversion between ISO Alpha-2, Alpha-3, and Numeric Country Codes. 

# Example uses:
## Validation
### ISO Alpha-2
```
1> countries_erlang:is_alpha_2(<<"US">>).
true
2> countries_erlang:is_alpha_2("US").
true
3> countries_erlang:is_alpha_2(<<"Us">>).
true
4> countries_erlang:is_alpha_2("us").
true
```
### ISO Alpha-3
```
1> countries_erlang:is_alpha_3(<<"USA">>).
true
2> countries_erlang:is_alpha_3("USA").
true
3> countries_erlang:is_alpha_3(<<"Usa">>).
true
4> countries_erlang:is_alpha_3("usa").
true
```
### Country (Checks for both ISO Alpha-2 and ISO Alpha-3)
```
1> countries_erlang:is_country(<<"US">>).
true
2> countries_erlang:is_country("USA").
true
3> countries_erlang:is_country(<<"Usa">>).
true
4> countries_erlang:is_country("us").
true
```
## Conversion
### Country name
```
1> countries_erlang:to_country_name("US").
<<"United States of America">>
2> countries_erlang:to_country_name(<<"USA">>).
<<"United States of America">>
```
### United Nations numerical code M49 for countries
```
1> countries_erlang:to_numerical_code("US").
840
2> countries_erlang:to_numerical_code("<<USA>>").
840
```
### ISO Alpha-2
```
1> countries_erlang:to_alpha_2("USA").
<<"US">>
2> countries_erlang:to_alpha_2("<<USA>>").
<<"US">>
```
### ISO Alpha-3
```
1> countries_erlang:to_alpha_3("US").
<<"USA">>
2> countries_erlang:to_alpha_3("<<US>>").
<<"USA">>
```
