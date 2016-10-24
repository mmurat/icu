-module(mn).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").
-export([month_report/2]).


	month_report(Year, Month) ->
		[ {{Year, Month, Day}, day_report(Year, Month, Day)} || Day <- lists:seq(1,31), calendar:valid_date(Year, Month, Day) ].

	day_report(Year, Month, Day) ->
		F = fun() ->
			Q = qlc:q([
									{
										{ province, I#icu.province },
										{ hospital, I#icu.hospital },
										{ insurance, I#icu.insurance },
										{ icu_type, I#icu.icu_type },
										{ success, I#icu.success },
										{ user, I#icu.user }
									}
									||
										I <- mnesia:table(icu),
										 day(I#icu.date, {Year, Month, Day})
								]),
			qlc:e(Q)
  	end,
	  {atomic, List} = mnesia:transaction(F),
		List.

day( { {Year, Month, Day}, _}, {Year, Month, Day} ) ->
	true;
day(_, _) ->
	false.
