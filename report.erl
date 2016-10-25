-module(report).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").
-export([out/2]).

  header() ->

    {'head', [], [
      {'link',  [{'rel', "stylesheet"}, {'href', "css/bootstrap.min.css"}]},
      {'link',  [{'rel', "stylesheet"}, {'href', "css/style.css"}]}
      ]}.

  out(Arg) ->

    Method = method(Arg),
    handle(Method, Arg).

  handle('GET', Arg,  ) ->

    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    io:format("~p", [Path]),
    handle('GET', Arg, Path);

  handle('GET', Arg, [api, report, Year, Month]) ->
      month_report(Year, Month).



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
