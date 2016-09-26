%% -*- coding: utf-8 -*-

-module(icu).
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").

-export([init/0, insert/0]).

-define(INSURANCE, [
	#insurance{ code = 0, name = <<"Güvence bilinmiyor"/utf8>> },
	#insurance{ code = 1, name = <<"SGK"/utf8>> },
	#insurance{ code = 2, name = <<"SSK"/utf8>> },
	#insurance{ code = 3, name = <<"Emekli Sandığı"/utf8>> },
	#insurance{ code = 4, name = <<"Bağkur"/utf8>> },
	#insurance{ code = 5, name = <<"Yeşilkart"/utf8>> },
	#insurance{ code = 6, name = <<"18 yaş altı"/utf8>> },
	#insurance{ code = 10, name = <<"Güvencesiz"/utf8>> },
	#insurance{ code = 11, name = <<"Suriyeli"/utf8>>}
]).

-define(ICU_TYPE, [
	#icu_type{ code = 0, name = <<"Yoğunbakım isteği yok/bilinmiyor"/utf8>> },
	#icu_type{ code = 1, name = <<"Yenidoğan"/utf8>> },
	#icu_type{ code = 2, name = <<"Çocuk"/utf8>> },
	#icu_type{ code = 3, name = <<"Dahiliye"/utf8>> },
	#icu_type{ code = 4, name = <<"Reanimasyon"/utf8>> },
	#icu_type{ code = 5, name = <<"Nöroloji"/utf8>> },
	#icu_type{ code = 6, name = <<"Beyin Cerrahi"/utf8>> },
	#icu_type{ code = 7, name = <<"Koroner"/utf8>> },
	#icu_type{ code = 8, name = <<"KVC"/utf8>> },
	#icu_type{ code = 9, name = <<"Yanık"/utf8>> },
	#icu_type{ code = 10, name = <<"Mikrocerrahi"/utf8>> }
]).

-define(HOSPITAL, [
		#hospital{ code = 0, name = <<"Hastane bilinmiyor/öğrenilemedi"/utf8>> },
		#hospital{ code = 1, name = <<"ADH"/utf8>> },
		#hospital{ code = 2, name = <<"ÇDH"/utf8>> },
		#hospital{ code = 3, name = <<"Numune"/utf8>> },
		#hospital{ code = 4, name = <<"Balcalı"/utf8>> },
		#hospital{ code = 5, name = <<"Marsa"/utf8>> },
		#hospital{ code = 6, name = <<"Meydan"/utf8>> },
		#hospital{ code = 7, name = <<"SUH"/utf8>> },
		#hospital{ code = 8, name = <<"Y. Başkent"/utf8>> },
		#hospital{ code = 9, name = <<"S. Başkent"/utf8>> },
		#hospital{ code = 10, name = <<"Medline"/utf8>> },
		#hospital{ code = 11, name = <<"Ortadoğu"/utf8>> },
		#hospital{ code = 12, name = <<"Ö. Adana"/utf8>> },
		#hospital{ code = 13, name = <<"G. Adana"/utf8>> },
		#hospital{ code = 14, name = <<"Algomed"/utf8>> },
		#hospital{ code = 15, name = <<"Acıbadem"/utf8>> },
		#hospital{ code = 16, name = <<"Metro"/utf8>> }
	]).

-define(PROVINCE, [
	 #province{ code = 0, name = <<"İl bilinmiyor/öğrenilemedi"/utf8>> },
	 #province{ code = 1, name = <<"Adana"/utf8>> },
	 #province{ code = 2, name = <<"Adıyaman"/utf8>> },
	 #province{ code = 3, name = <<"Afyonkarahisar"/utf8>> },
	 #province{ code = 4, name = <<"Ağrı"/utf8>> },
	 #province{ code = 5, name = <<"Amasya"/utf8>> },
	 #province{ code = 6, name = <<"Ankara"/utf8>> },
	 #province{ code = 7, name = <<"Antalya"/utf8>> },
	 #province{ code = 8, name = <<"Artvin"/utf8>> },
	 #province{ code = 9, name = <<"Aydın"/utf8>> },
	 #province{ code = 10, name = <<"Balıkesir"/utf8>> },
	 #province{ code = 11, name = <<"Bilecik"/utf8>> },
	 #province{ code = 12, name = <<"Bingöl"/utf8>> },
	 #province{ code = 13, name = <<"Bitlis"/utf8>> },
	 #province{ code = 14, name = <<"Bolu"/utf8>> },
	 #province{ code = 15, name = <<"Burdur"/utf8>> },
	 #province{ code = 16, name = <<"Bursa"/utf8>> },
	 #province{ code = 17, name = <<"Çanakkale"/utf8>> },
	 #province{ code = 18, name = <<"Çankırı"/utf8>> },
	 #province{ code = 19, name = <<"Çorum"/utf8>> },
	 #province{ code = 20, name = <<"Denizli"/utf8>> },
	 #province{ code = 21, name = <<"Diyarbakır"/utf8>> },
	 #province{ code = 22, name = <<"Edirne"/utf8>> },
	 #province{ code = 23, name = <<"Elazığ"/utf8>> },
	 #province{ code = 24, name = <<"Erzincan"/utf8>> },
	 #province{ code = 25, name = <<"Erzurum"/utf8>> },
	 #province{ code = 26, name = <<"Eskişehir"/utf8>> },
	 #province{ code = 27, name = <<"Gaziantep"/utf8>> },
	 #province{ code = 28, name = <<"Giresun"/utf8>> },
	 #province{ code = 29, name = <<"Gümüşhane"/utf8>> },
	 #province{ code = 30, name = <<"Hakkâri"/utf8>> },
	 #province{ code = 31, name = <<"Hatay"/utf8>> },
	 #province{ code = 32, name = <<"Isparta"/utf8>> },
	 #province{ code = 33, name = <<"Mersin"/utf8>> },
	 #province{ code = 34, name = <<"İstanbul"/utf8>> },
	 #province{ code = 35, name = <<"İzmir"/utf8>> },
	 #province{ code = 36, name = <<"Kars"/utf8>> },
	 #province{ code = 37, name = <<"Kastamonu"/utf8>> },
	 #province{ code = 38, name = <<"Kayseri"/utf8>> },
	 #province{ code = 39, name = <<"Kırklareli"/utf8>> },
	 #province{ code = 40, name = <<"Kırşehir"/utf8>> },
	 #province{ code = 41, name = <<"Kocaeli"/utf8>> },
	 #province{ code = 42, name = <<"Konya"/utf8>> },
	 #province{ code = 43, name = <<"Kütahya"/utf8>> },
	 #province{ code = 44, name = <<"Malatya"/utf8>> },
	 #province{ code = 45, name = <<"Manisa"/utf8>> },
	 #province{ code = 46, name = <<"Kahramanmaraş"/utf8>> },
	 #province{ code = 47, name = <<"Mardin"/utf8>> },
	 #province{ code = 48, name = <<"Muğla"/utf8>> },
	 #province{ code = 49, name = <<"Muş"/utf8>> },
	 #province{ code = 50, name = <<"Nevşehir"/utf8>> },
	 #province{ code = 51, name = <<"Niğde"/utf8>> },
	 #province{ code = 52, name = <<"Ordu"/utf8>> },
	 #province{ code = 53, name = <<"Rize"/utf8>> },
	 #province{ code = 54, name = <<"Sakarya"/utf8>> },
	 #province{ code = 55, name = <<"Samsun"/utf8>> },
	 #province{ code = 56, name = <<"Siirt"/utf8>> },
	 #province{ code = 57, name = <<"Sinop"/utf8>> },
	 #province{ code = 58, name = <<"Sivas"/utf8>> },
	 #province{ code = 59, name = <<"Tekirdağ"/utf8>> },
	 #province{ code = 60, name = <<"Tokat"/utf8>> },
	 #province{ code = 61, name = <<"Trabzon"/utf8>> },
	 #province{ code = 62, name = <<"Tunceli"/utf8>> },
	 #province{ code = 63, name = <<"Şanlıurfa"/utf8>> },
	 #province{ code = 64, name = <<"Uşak"/utf8>> },
	 #province{ code = 65, name = <<"Van"/utf8>> },
	 #province{ code = 66, name = <<"Yozgat"/utf8>> },
	 #province{ code = 67, name = <<"Zonguldak"/utf8>> },
	 #province{ code = 68, name = <<"Aksaray"/utf8>> },
	 #province{ code = 69, name = <<"Bayburt"/utf8>> },
	 #province{ code = 70, name = <<"Karaman"/utf8>> },
	 #province{ code = 71, name = <<"Kırıkkale"/utf8>> },
	 #province{ code = 72, name = <<"Batman"/utf8>> },
	 #province{ code = 73, name = <<"Şırnak"/utf8>> },
	 #province{ code = 74, name = <<"Bartın"/utf8>> },
	 #province{ code = 75, name = <<"Ardahan"/utf8>> },
	 #province{ code = 76, name = <<"Iğdır"/utf8>> },
	 #province{ code = 77, name = <<"Yalova"/utf8>> },
	 #province{ code = 78, name = <<"Karabük"/utf8>> },
	 #province{ code = 79, name = <<"Kilis"/utf8>> },
	 #province{ code = 80, name = <<"Osmaniye"/utf8>> },
	 #province{ code = 81, name = <<"Düzce"/utf8>> }
 ]).


init() ->
	mnesia:create_table(user,
		[{attributes, record_info(fields, user)}]),

	mnesia:create_table(hospital,
		[{attributes, record_info(fields, hospital)}]),

	mnesia:create_table(province,
		[{attributes, record_info(fields, province)}]),

	mnesia:create_table(icu_type,
		[{attributes, record_info(fields, icu_type)}]),

	mnesia:create_table(insurance,
		[{attributes, record_info(fields, insurance)}]),

	mnesia:create_table(icu,
		[{attributes, record_info(fields, icu)}]),

	% mnesia:create_table(icu_province,
	% 	[{attributes, record_info(fields, icu_province)}]),
	%
	% mnesia:create_table(icu_hospital,
	% 	[{attributes, record_info(fields, icu_hospital)}]),
	%
	% mnesia:create_table(icu_insurance,
	% 	[{attributes, record_info(fields, icu_insurance)}]),
	%
	% mnesia:create_table(icu_search,
	% 	[{attributes, record_info(fields, icu_search)}]),

	mnesia:change_table_copy_type(user, node(), disc_copies),
	mnesia:change_table_copy_type(hospital, node(), disc_copies),
	mnesia:change_table_copy_type(province, node(), disc_copies),
	mnesia:change_table_copy_type(icu_type, node(), disc_copies),
	mnesia:change_table_copy_type(insurance, node(), disc_copies),
	mnesia:change_table_copy_type(icu, node(), disc_copies).
	% mnesia:change_table_copy_type(icu_province, node(), disc_copies),
	% mnesia:change_table_copy_type(icu_hospital, node(), disc_copies),
	% mnesia:change_table_copy_type(icu_insurance, node(), disc_copies),
	% mnesia:change_table_copy_type(icu_search, node(), disc_copies).

insert() ->

	Fun = fun() ->
		write(?PROVINCE),
		write(?HOSPITAL),
		write(?ICU_TYPE),
		write(?INSURANCE)
	end,
	mnesia:transaction(Fun).

write([]) ->
	ok;

write([H|T]) ->
	mnesia:write(H),
	write(T).

% print() ->
% 	print(?PROVINCE).

% print([]) ->
% 	ok;

% print([H|T]) ->
% 	io:format("{~w ~ts}~n", [H#province.code, H#province.name]),
% 	print(T).

% findProvince(Code) ->
%     Pro = lists:keysearch(Code, #province.code, ?PROVINCE),
%     case Pro of
%     	{value, {province, _, Name}} ->
%     		io:format("~ts~n", [Name]);
%     	false ->
%     		notfound
%     	end.
