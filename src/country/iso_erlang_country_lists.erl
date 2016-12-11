%%%-------------------------------------------------------------------
%%% @doc Returns list of all countries
%%% @end
%%%-------------------------------------------------------------------
-module(iso_erlang_country_lists).

%% API
-export([
    get_alpha_2_country_list/0,
    get_alpha_3_country_list/0,
    get_country_name_list/0
]).

get_alpha_2_country_list() ->
    [<<"AD">>,
     <<"AE">>,
     <<"AF">>,
     <<"AG">>,
     <<"AI">>,
     <<"AL">>,
     <<"AM">>,
     <<"AO">>,
     <<"AQ">>,
     <<"AR">>,
     <<"AS">>,
     <<"AT">>,
     <<"AU">>,
     <<"AW">>,
     <<"AX">>,
     <<"AZ">>,
     <<"BA">>,
     <<"BB">>,
     <<"BD">>,
     <<"BE">>,
     <<"BF">>,
     <<"BG">>,
     <<"BH">>,
     <<"BI">>,
     <<"BJ">>,
     <<"BL">>,
     <<"BM">>,
     <<"BN">>,
     <<"BO">>,
     <<"BQ">>,
     <<"BR">>,
     <<"BS">>,
     <<"BT">>,
     <<"BV">>,
     <<"BW">>,
     <<"BY">>,
     <<"BZ">>,
     <<"CA">>,
     <<"CC">>,
     <<"CD">>,
     <<"CF">>,
     <<"CG">>,
     <<"CH">>,
     <<"CI">>,
     <<"CK">>,
     <<"CL">>,
     <<"CM">>,
     <<"CN">>,
     <<"CO">>,
     <<"CR">>,
     <<"CU">>,
     <<"CV">>,
     <<"CW">>,
     <<"CX">>,
     <<"CY">>,
     <<"CZ">>,
     <<"DE">>,
     <<"DJ">>,
     <<"DK">>,
     <<"DM">>,
     <<"DO">>,
     <<"DZ">>,
     <<"EC">>,
     <<"EE">>,
     <<"EG">>,
     <<"EH">>,
     <<"ER">>,
     <<"ES">>,
     <<"ET">>,
     <<"FI">>,
     <<"FJ">>,
     <<"FK">>,
     <<"FM">>,
     <<"FO">>,
     <<"FR">>,
     <<"GA">>,
     <<"GB">>,
     <<"GD">>,
     <<"GE">>,
     <<"GF">>,
     <<"GG">>,
     <<"GH">>,
     <<"GI">>,
     <<"GL">>,
     <<"GM">>,
     <<"GN">>,
     <<"GP">>,
     <<"GQ">>,
     <<"GR">>,
     <<"GS">>,
     <<"GT">>,
     <<"GU">>,
     <<"GW">>,
     <<"GY">>,
     <<"HK">>,
     <<"HM">>,
     <<"HN">>,
     <<"HR">>,
     <<"HT">>,
     <<"HU">>,
     <<"ID">>,
     <<"IE">>,
     <<"IL">>,
     <<"IM">>,
     <<"IN">>,
     <<"IO">>,
     <<"IQ">>,
     <<"IR">>,
     <<"IS">>,
     <<"IT">>,
     <<"JE">>,
     <<"JM">>,
     <<"JO">>,
     <<"JP">>,
     <<"KE">>,
     <<"KG">>,
     <<"KH">>,
     <<"KI">>,
     <<"KM">>,
     <<"KN">>,
     <<"KP">>,
     <<"KR">>,
     <<"KW">>,
     <<"KY">>,
     <<"KZ">>,
     <<"LA">>,
     <<"LB">>,
     <<"LC">>,
     <<"LI">>,
     <<"LK">>,
     <<"LR">>,
     <<"LS">>,
     <<"LT">>,
     <<"LU">>,
     <<"LV">>,
     <<"LY">>,
     <<"MA">>,
     <<"MC">>,
     <<"MD">>,
     <<"ME">>,
     <<"MF">>,
     <<"MG">>,
     <<"MH">>,
     <<"MK">>,
     <<"ML">>,
     <<"MM">>,
     <<"MN">>,
     <<"MO">>,
     <<"MP">>,
     <<"MQ">>,
     <<"MR">>,
     <<"MS">>,
     <<"MT">>,
     <<"MU">>,
     <<"MV">>,
     <<"MW">>,
     <<"MX">>,
     <<"MY">>,
     <<"MZ">>,
     <<"NA">>,
     <<"NC">>,
     <<"NE">>,
     <<"NF">>,
     <<"NG">>,
     <<"NI">>,
     <<"NL">>,
     <<"NO">>,
     <<"NP">>,
     <<"NR">>,
     <<"NU">>,
     <<"NZ">>,
     <<"OM">>,
     <<"PA">>,
     <<"PE">>,
     <<"PF">>,
     <<"PG">>,
     <<"PH">>,
     <<"PK">>,
     <<"PL">>,
     <<"PM">>,
     <<"PN">>,
     <<"PR">>,
     <<"PS">>,
     <<"PT">>,
     <<"PW">>,
     <<"PY">>,
     <<"QA">>,
     <<"RE">>,
     <<"RO">>,
     <<"RS">>,
     <<"RU">>,
     <<"RW">>,
     <<"SA">>,
     <<"SB">>,
     <<"SC">>,
     <<"SD">>,
     <<"SE">>,
     <<"SG">>,
     <<"SH">>,
     <<"SI">>,
     <<"SJ">>,
     <<"SK">>,
     <<"SL">>,
     <<"SM">>,
     <<"SN">>,
     <<"SO">>,
     <<"SR">>,
     <<"SS">>,
     <<"ST">>,
     <<"SV">>,
     <<"SX">>,
     <<"SY">>,
     <<"SZ">>,
     <<"TC">>,
     <<"TD">>,
     <<"TF">>,
     <<"TG">>,
     <<"TH">>,
     <<"TJ">>,
     <<"TK">>,
     <<"TL">>,
     <<"TM">>,
     <<"TN">>,
     <<"TO">>,
     <<"TR">>,
     <<"TT">>,
     <<"TV">>,
     <<"TW">>,
     <<"TZ">>,
     <<"UA">>,
     <<"UG">>,
     <<"UM">>,
     <<"US">>,
     <<"UY">>,
     <<"UZ">>,
     <<"VA">>,
     <<"VC">>,
     <<"VE">>,
     <<"VG">>,
     <<"VI">>,
     <<"VN">>,
     <<"VU">>,
     <<"WF">>,
     <<"WS">>,
     <<"YE">>,
     <<"YT">>,
     <<"ZA">>,
     <<"ZM">>,
     <<"ZW">>].

get_alpha_3_country_list() ->
    [<<"AFG">>,
     <<"ABW">>,
     <<"AGO">>,
     <<"AIA">>,
     <<"ALA">>,
     <<"ALB">>,
     <<"AND">>,
     <<"ARE">>,
     <<"ARG">>,
     <<"ARM">>,
     <<"ASM">>,
     <<"ATA">>,
     <<"ATF">>,
     <<"ATG">>,
     <<"AUS">>,
     <<"AUT">>,
     <<"AZE">>,
     <<"BDI">>,
     <<"BEL">>,
     <<"BEN">>,
     <<"BES">>,
     <<"BFA">>,
     <<"BGD">>,
     <<"BGR">>,
     <<"BHR">>,
     <<"BHS">>,
     <<"BIH">>,
     <<"BLM">>,
     <<"BLR">>,
     <<"BLZ">>,
     <<"BMU">>,
     <<"BOL">>,
     <<"BRA">>,
     <<"BRB">>,
     <<"BRN">>,
     <<"BTN">>,
     <<"BVT">>,
     <<"BWA">>,
     <<"CAF">>,
     <<"CAN">>,
     <<"CCK">>,
     <<"CHE">>,
     <<"CHL">>,
     <<"CHN">>,
     <<"CIV">>,
     <<"CMR">>,
     <<"COD">>,
     <<"COG">>,
     <<"COK">>,
     <<"COL">>,
     <<"COM">>,
     <<"CPV">>,
     <<"CRI">>,
     <<"CUB">>,
     <<"CUW">>,
     <<"CXR">>,
     <<"CYM">>,
     <<"CYP">>,
     <<"CZE">>,
     <<"DEU">>,
     <<"DJI">>,
     <<"DMA">>,
     <<"DNK">>,
     <<"DOM">>,
     <<"DZA">>,
     <<"ECU">>,
     <<"EGY">>,
     <<"ERI">>,
     <<"ESH">>,
     <<"ESP">>,
     <<"EST">>,
     <<"ETH">>,
     <<"FIN">>,
     <<"FJI">>,
     <<"FLK">>,
     <<"FRA">>,
     <<"FRO">>,
     <<"FSM">>,
     <<"GAB">>,
     <<"GBR">>,
     <<"GEO">>,
     <<"GGY">>,
     <<"GHA">>,
     <<"GIB">>,
     <<"GIN">>,
     <<"GLP">>,
     <<"GMB">>,
     <<"GNB">>,
     <<"GNQ">>,
     <<"GRC">>,
     <<"GRD">>,
     <<"GRL">>,
     <<"GTM">>,
     <<"GUF">>,
     <<"GUM">>,
     <<"GUY">>,
     <<"HKG">>,
     <<"HMD">>,
     <<"HND">>,
     <<"HRV">>,
     <<"HTI">>,
     <<"HUN">>,
     <<"IDN">>,
     <<"IMN">>,
     <<"IND">>,
     <<"IOT">>,
     <<"IRL">>,
     <<"IRN">>,
     <<"IRQ">>,
     <<"ISL">>,
     <<"ISR">>,
     <<"ITA">>,
     <<"JAM">>,
     <<"JEY">>,
     <<"JOR">>,
     <<"JPN">>,
     <<"KAZ">>,
     <<"KEN">>,
     <<"KGZ">>,
     <<"KHM">>,
     <<"KIR">>,
     <<"KNA">>,
     <<"KOR">>,
     <<"KWT">>,
     <<"LAO">>,
     <<"LBN">>,
     <<"LBR">>,
     <<"LBY">>,
     <<"LCA">>,
     <<"LIE">>,
     <<"LKA">>,
     <<"LSO">>,
     <<"LTU">>,
     <<"LUX">>,
     <<"LVA">>,
     <<"MAC">>,
     <<"MAF">>,
     <<"MAR">>,
     <<"MCO">>,
     <<"MDA">>,
     <<"MDG">>,
     <<"MDV">>,
     <<"MEX">>,
     <<"MHL">>,
     <<"MKD">>,
     <<"MLI">>,
     <<"MLT">>,
     <<"MMR">>,
     <<"MNE">>,
     <<"MNG">>,
     <<"MNP">>,
     <<"MOZ">>,
     <<"MRT">>,
     <<"MSR">>,
     <<"MTQ">>,
     <<"MUS">>,
     <<"MWI">>,
     <<"MYS">>,
     <<"MYT">>,
     <<"NAM">>,
     <<"NCL">>,
     <<"NER">>,
     <<"NFK">>,
     <<"NGA">>,
     <<"NIC">>,
     <<"NIU">>,
     <<"NLD">>,
     <<"NOR">>,
     <<"NPL">>,
     <<"NRU">>,
     <<"NZL">>,
     <<"OMN">>,
     <<"PAK">>,
     <<"PAN">>,
     <<"PCN">>,
     <<"PER">>,
     <<"PHL">>,
     <<"PLW">>,
     <<"PNG">>,
     <<"POL">>,
     <<"PRI">>,
     <<"PRK">>,
     <<"PRT">>,
     <<"PRY">>,
     <<"PSE">>,
     <<"PYF">>,
     <<"QAT">>,
     <<"REU">>,
     <<"ROU">>,
     <<"RUS">>,
     <<"RWA">>,
     <<"SAU">>,
     <<"SDN">>,
     <<"SEN">>,
     <<"SGP">>,
     <<"SGS">>,
     <<"SHN">>,
     <<"SJM">>,
     <<"SLB">>,
     <<"SLE">>,
     <<"SLV">>,
     <<"SMR">>,
     <<"SOM">>,
     <<"SPM">>,
     <<"SRB">>,
     <<"SSD">>,
     <<"STP">>,
     <<"SUR">>,
     <<"SVK">>,
     <<"SVN">>,
     <<"SWE">>,
     <<"SWZ">>,
     <<"SXM">>,
     <<"SYC">>,
     <<"SYR">>,
     <<"TCA">>,
     <<"TCD">>,
     <<"TGO">>,
     <<"THA">>,
     <<"TJK">>,
     <<"TKL">>,
     <<"TKM">>,
     <<"TLS">>,
     <<"TON">>,
     <<"TTO">>,
     <<"TUN">>,
     <<"TUR">>,
     <<"TUV">>,
     <<"TWN">>,
     <<"TZA">>,
     <<"UGA">>,
     <<"UKR">>,
     <<"UMI">>,
     <<"URY">>,
     <<"USA">>,
     <<"UZB">>,
     <<"VAT">>,
     <<"VCT">>,
     <<"VEN">>,
     <<"VGB">>,
     <<"VIR">>,
     <<"VNM">>,
     <<"VUT">>,
     <<"WLF">>,
     <<"WSM">>,
     <<"YEM">>,
     <<"ZAF">>,
     <<"ZMB">>,
     <<"ZWE">>].

get_country_name_list() ->
    [<<"Afghanistan"/utf8>>,
     <<"Aland Islands"/utf8>>,
     <<"Albania"/utf8>>,
     <<"Algeria"/utf8>>,
     <<"American Samoa"/utf8>>,
     <<"Andorra"/utf8>>,
     <<"Angola"/utf8>>,
     <<"Anguilla"/utf8>>,
     <<"Antarctica"/utf8>>,
     <<"Antigua and Barbuda"/utf8>>,
     <<"Argentina"/utf8>>,
     <<"Armenia"/utf8>>,
     <<"Aruba"/utf8>>,
     <<"Australia"/utf8>>,
     <<"Austria"/utf8>>,
     <<"Azerbaijan"/utf8>>,
     <<"Bahamas (the)"/utf8>>,
     <<"Bahrain"/utf8>>,
     <<"Bangladesh"/utf8>>,
     <<"Barbados"/utf8>>,
     <<"Belarus"/utf8>>,
     <<"Belgium"/utf8>>,
     <<"Belize"/utf8>>,
     <<"Benin"/utf8>>,
     <<"Bermuda"/utf8>>,
     <<"Bhutan"/utf8>>,
     <<"Bolivia (Plurinational State of)"/utf8>>,
     <<"Bonaire, Sint Eustatius and Saba"/utf8>>,
     <<"Bosnia and Herzegovina"/utf8>>,
     <<"Botswana"/utf8>>,
     <<"Bouvet Island"/utf8>>,
     <<"Brazil"/utf8>>,
     <<"British Indian Ocean Territory (the)"/utf8>>,
     <<"Virgin Islands (British)"/utf8>>,
     <<"Brunei Darussalam"/utf8>>,
     <<"Bulgaria"/utf8>>,
     <<"Burkina Faso"/utf8>>,
     <<"Burundi"/utf8>>,
     <<"Cambodia"/utf8>>,
     <<"Cameroon"/utf8>>,
     <<"Canada"/utf8>>,
     <<"Cabo Verde"/utf8>>,
     <<"Cayman Islands (the)"/utf8>>,
     <<"Central African Republic (the)"/utf8>>,
     <<"Chad"/utf8>>,
     <<"Chile"/utf8>>,
     <<"China"/utf8>>,
     <<"Christmas Island"/utf8>>,
     <<"Cocos (Keeling) Islands (the)"/utf8>>,
     <<"Colombia"/utf8>>,
     <<"Comoros (the)"/utf8>>,
     <<"Congo (the)"/utf8>>,
     <<"Cook Islands (the)"/utf8>>,
     <<"Costa Rica"/utf8>>,
     <<"Croatia"/utf8>>,
     <<"Cuba"/utf8>>,
     <<"Curaçao"/utf8>>,
     <<"Cyprus"/utf8>>,
     <<"Czechia"/utf8>>,
     <<"Côte d'Ivoire"/utf8>>,
     <<"Korea (the Democratic People's Republic of)"/utf8>>,
     <<"Congo (the Democratic Republic of the)"/utf8>>,
     <<"Denmark"/utf8>>,
     <<"Djibouti"/utf8>>,
     <<"Dominica"/utf8>>,
     <<"Dominican Republic (the)"/utf8>>,
     <<"Ecuador"/utf8>>,
     <<"Egypt"/utf8>>,
     <<"El Salvador"/utf8>>,
     <<"Equatorial Guinea"/utf8>>,
     <<"Eritrea"/utf8>>,
     <<"Estonia"/utf8>>,
     <<"Ethiopia"/utf8>>,
     <<"Falkland Islands (the) [Malvinas]"/utf8>>,
     <<"Faroe Islands (the)"/utf8>>,
     <<"Micronesia (Federated States of)"/utf8>>,
     <<"Fiji"/utf8>>,
     <<"Finland"/utf8>>,
     <<"France"/utf8>>,
     <<"French Guiana"/utf8>>,
     <<"French Polynesia"/utf8>>,
     <<"French Southern Territories (the)"/utf8>>,
     <<"Gabon"/utf8>>,
     <<"Gambia (the)"/utf8>>,
     <<"Georgia"/utf8>>,
     <<"Germany"/utf8>>,
     <<"Ghana"/utf8>>,
     <<"Gibraltar"/utf8>>,
     <<"Greece"/utf8>>,
     <<"Greenland"/utf8>>,
     <<"Grenada"/utf8>>,
     <<"Guadeloupe"/utf8>>,
     <<"Guam"/utf8>>,
     <<"Guatemala"/utf8>>,
     <<"Guernsey"/utf8>>,
     <<"Guinea"/utf8>>,
     <<"Guinea-Bissau"/utf8>>,
     <<"Guyana"/utf8>>,
     <<"Haiti"/utf8>>,
     <<"Heard Island and McDonald Islands"/utf8>>,
     <<"Holy See (the)"/utf8>>,
     <<"Honduras"/utf8>>,
     <<"Hong Kong"/utf8>>,
     <<"Hungary"/utf8>>,
     <<"Iceland"/utf8>>,
     <<"India"/utf8>>,
     <<"Indonesia"/utf8>>,
     <<"Iran (Islamic Republic of)"/utf8>>,
     <<"Iraq"/utf8>>,
     <<"Ireland"/utf8>>,
     <<"Isle of Man"/utf8>>,
     <<"Israel"/utf8>>,
     <<"Italy"/utf8>>,
     <<"Jamaica"/utf8>>,
     <<"Japan"/utf8>>,
     <<"Jersey"/utf8>>,
     <<"Jordan"/utf8>>,
     <<"Kazakhstan"/utf8>>,
     <<"Kenya"/utf8>>,
     <<"Kiribati"/utf8>>,
     <<"Kuwait"/utf8>>,
     <<"Kyrgyzstan"/utf8>>,
     <<"Lao People's Democratic Republic (the)"/utf8>>,
     <<"Latvia"/utf8>>,
     <<"Lebanon"/utf8>>,
     <<"Lesotho"/utf8>>,
     <<"Liberia"/utf8>>,
     <<"Libya"/utf8>>,
     <<"Liechtenstein"/utf8>>,
     <<"Lithuania"/utf8>>,
     <<"Luxembourg"/utf8>>,
     <<"Macao"/utf8>>,
     <<"Madagascar"/utf8>>,
     <<"Malawi"/utf8>>,
     <<"Malaysia"/utf8>>,
     <<"Maldives"/utf8>>,
     <<"Mali"/utf8>>,
     <<"Malta"/utf8>>,
     <<"Marshall Islands (the)"/utf8>>,
     <<"Martinique"/utf8>>,
     <<"Mauritania"/utf8>>,
     <<"Mauritius"/utf8>>,
     <<"Mayotte"/utf8>>,
     <<"Mexico"/utf8>>,
     <<"Moldova (the Republic of)"/utf8>>,
     <<"Monaco"/utf8>>,
     <<"Mongolia"/utf8>>,
     <<"Montenegro"/utf8>>,
     <<"Montserrat"/utf8>>,
     <<"Morocco"/utf8>>,
     <<"Mozambique"/utf8>>,
     <<"Myanmar"/utf8>>,
     <<"Namibia"/utf8>>,
     <<"Nauru"/utf8>>,
     <<"Nepal"/utf8>>,
     <<"Netherlands (the)"/utf8>>,
     <<"New Caledonia"/utf8>>,
     <<"New Zealand"/utf8>>,
     <<"Nicaragua"/utf8>>,
     <<"Niger (the)"/utf8>>,
     <<"Nigeria"/utf8>>,
     <<"Niue"/utf8>>,
     <<"Norfolk Island"/utf8>>,
     <<"Northern Mariana Islands (the)"/utf8>>,
     <<"Norway"/utf8>>,
     <<"Oman"/utf8>>,
     <<"Pakistan"/utf8>>,
     <<"Palau"/utf8>>,
     <<"Palestine, State of"/utf8>>,
     <<"Panama"/utf8>>,
     <<"Papua New Guinea"/utf8>>,
     <<"Paraguay"/utf8>>,
     <<"Peru"/utf8>>,
     <<"Philippines (the)"/utf8>>,
     <<"Pitcairn"/utf8>>,
     <<"Poland"/utf8>>,
     <<"Portugal"/utf8>>,
     <<"Puerto Rico"/utf8>>,
     <<"Qatar"/utf8>>,
     <<"Korea (the Republic of)"/utf8>>,
     <<"Macedonia (the former Yugoslav Republic of)"/utf8>>,
     <<"Romania"/utf8>>,
     <<"Russian Federation (the)"/utf8>>,
     <<"Rwanda"/utf8>>,
     <<"Réunion"/utf8>>,
     <<"Saint Helena, Ascension and Tristan da Cunha"/utf8>>,
     <<"Saint Kitts and Nevis"/utf8>>,
     <<"Saint Lucia"/utf8>>,
     <<"Saint Pierre and Miquelon"/utf8>>,
     <<"Saint Vincent and the Grenadines"/utf8>>,
     <<"Saint Barthélemy"/utf8>>,
     <<"Saint Martin (French part)"/utf8>>,
     <<"Samoa"/utf8>>,
     <<"San Marino"/utf8>>,
     <<"Sao Tome and Principe"/utf8>>,
     <<"Saudi Arabia"/utf8>>,
     <<"Senegal"/utf8>>,
     <<"Serbia"/utf8>>,
     <<"Seychelles"/utf8>>,
     <<"Sierra Leone"/utf8>>,
     <<"Singapore"/utf8>>,
     <<"Sint Maarten (Dutch part)"/utf8>>,
     <<"Slovakia"/utf8>>,
     <<"Slovenia"/utf8>>,
     <<"Solomon Islands"/utf8>>,
     <<"Somalia"/utf8>>,
     <<"South Africa"/utf8>>,
     <<"South Georgia and the South Sandwich Islands"/utf8>>,
     <<"South Sudan"/utf8>>,
     <<"Spain"/utf8>>,
     <<"Sri Lanka"/utf8>>,
     <<"Sudan (the)"/utf8>>,
     <<"Suriname"/utf8>>,
     <<"Svalbard and Jan Mayen"/utf8>>,
     <<"Swaziland"/utf8>>,
     <<"Sweden"/utf8>>,
     <<"Switzerland"/utf8>>,
     <<"Syrian Arab Republic"/utf8>>,
     <<"Taiwan (Province of China)"/utf8>>,
     <<"Tajikistan"/utf8>>,
     <<"Tanzania, United Republic of"/utf8>>,
     <<"Thailand"/utf8>>,
     <<"Timor-Leste"/utf8>>,
     <<"Togo"/utf8>>,
     <<"Tokelau"/utf8>>,
     <<"Tonga"/utf8>>,
     <<"Trinidad and Tobago"/utf8>>,
     <<"Tunisia"/utf8>>,
     <<"Turkey"/utf8>>,
     <<"Turkmenistan"/utf8>>,
     <<"Turks and Caicos Islands (the)"/utf8>>,
     <<"Tuvalu"/utf8>>,
     <<"Uganda"/utf8>>,
     <<"Ukraine"/utf8>>,
     <<"United Arab Emirates (the)"/utf8>>,
     <<"United Kingdom of Great Britain and Northern Ireland (the)"/utf8>>,
     <<"United States Minor Outlying Islands (the)"/utf8>>,
     <<"United States of America (the)"/utf8>>,
     <<"Uruguay"/utf8>>,
     <<"Virgin Islands (U.S.)"/utf8>>,
     <<"Uzbekistan"/utf8>>,
     <<"Vanuatu"/utf8>>,
     <<"Venezuela (Bolivarian Republic of)"/utf8>>,
     <<"Viet Nam"/utf8>>,
     <<"Wallis and Futuna"/utf8>>,
     <<"Western Sahara*"/utf8>>,
     <<"Yemen"/utf8>>,
     <<"Zambia"/utf8>>,
     <<"Zimbabwe"/utf8>>].
