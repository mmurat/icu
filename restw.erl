
%% -*- coding: utf-8 -*-

-module(restw).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").

-export([out/1]).

formatDate( {_Date={Year,Month,Day},_Time={Hour,Minutes, _Seconds}}) ->
    list_to_bitstring(integer_to_list(Day) ++ "/" ++ integer_to_list(Month) ++ "/" ++
    integer_to_list(Year) ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minutes)).

addIcu( Code, Name, Province, Hospital, Insurance, IcuType, Success, User ) ->

  NewRec =
    #icu{ code = Code,
      name = Name,
      province = Province,
      hospital = Hospital,
      insurance = Insurance,
      icu_type = IcuType,
      success = Success,
      user = User,
      date = erlang:localtime()},

  io:format("~p:~p Adding Icu ~p~n", [?MODULE, ?LINE, NewRec]),
  Add = fun() ->
          mnesia:write(NewRec)
        end,
  {atomic, _Rec} = mnesia:transaction(Add),
  NewRec.

convert_to_json(Lines) ->
  Data    =  [{obj,
     [ { code,     Line#icu.code },
       { name,     Line#icu.name  },
       { province, Line#icu.province },
       { hospital, Line#icu.hospital },
       { insurance, Line#icu.insurance },
       { icu_type, Line#icu.icu_type },
       { user, Line#icu.user },
       { success, Line#icu.success },
       { date, formatDate(Line#icu.date)  }
       ]} || Line <- Lines],

   JsonData  = {obj, [{data, Data}]},
   rfc4627:encode(JsonData).

method(Arg) ->
  Rec = Arg#arg.req,
  Rec#http_request.method.

out(Arg) ->
  Method = method(Arg),
  io:format("~p: ~p ~p Request ~n", [?MODULE, ?LINE, Method]),
  handle(Method, Arg).

handle('GET', Arg) ->

    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    io:format("~p", [Path]),

    getIcu('GET', Arg, Path);

%    {html, Json};

handle('POST', Arg) ->

    io:format("~n~p:~p POST request ~p~n", [?MODULE, ?LINE, yaws_api:parse_post(Arg)]),
    Icu = yaws_api:parse_post(Arg),
    io:format("~n~p:~p POST request ~p~n", [?MODULE, ?LINE, Icu]),

  [ {"code", Code}, {"name", Name}, {"province", Province}, {"hospital", Hospital},
   {"icu_type", IcuType}, {"insurance", Insurance},  {"success", Success}, {"user", User}] = Icu,

    _Status     = addIcu(list_to_integer(Code), list_to_bitstring(Name), list_to_integer(Province),
                          list_to_integer(Hospital), list_to_integer(Insurance), list_to_integer(IcuType),
                              list_to_integer(Success), list_to_integer(User) ),

  JsonData  = {obj, [{data, {obj, [{"code", Code}]}}]},
  Json = rfc4627:encode(JsonData),

  {html, Json};

  handle('PUT', Arg) ->

    io:format("~p:~p PUT request ~p ~n",
      [?MODULE, ?LINE, yaws_api:parse_post(Arg)]),
    Icu = yaws_api:parse_post(Arg),
    [ {"code", Code}, {"name", Name}, {"province", Province}, {"hospital", Hospital},
      {"icu_type", IcuType}, {"insurance", Insurance},  {"success", Success}, {"user", User}] = Icu,

    Fun = fun() ->
        mnesia:read({icu, list_to_integer(Code)})
      end,
    { atomic, [Record | _] } = mnesia:transaction(Fun),

  NewRec =
    #icu{ code = list_to_integer(Code),
      name = list_to_bitstring(Name),
      province = list_to_integer(Province),
      hospital = list_to_integer(Hospital),
      insurance = list_to_integer(Insurance),
      icu_type = list_to_integer(IcuType),
      success = list_to_integer(Success),
      user = list_to_integer(User),
      date = Record#icu.date
    },

    io:format("~p:~p Renaming ~p",
      [?MODULE, ?LINE, NewRec]),

    ChangeIcu         = fun() ->
                            mnesia:delete({icu, Code}),
                            mnesia:write(NewRec)
                          end,
    {atomic, _Rec}      = mnesia:transaction(ChangeIcu),
    [{status, 200},
      {html, Code}];

  handle('DELETE', Arg) ->

    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),

    ["api", Code]     = Path,


    io:format("~p:~p DELETE request ~p",
      [?MODULE, ?LINE, Code]),

    Delete              = fun() ->
      mnesia:delete({icu, list_to_integer(Code)})
                          end,

    Resp                = mnesia:transaction(Delete),

    case Resp of
      {atomic, ok} ->
        [{status, 204}];
      {_, Error} ->
        io:format("~p:~p Error ~p ",
          [?MODULE, ?LINE, Error]),
        [{status, 400},
          {html, Error}]
    end;

  handle(Method, _) ->
    [{error, "Unknown method " ++ Method},
      {status, 405},
      {header, "Allow: GET, HEAD, POST, PUT, DELETE"}].


  getIcu('GET', Arg, ["api"]) ->

      io:format("~n ~p:~p GET Request", [?MODULE, ?LINE]),

      Fun = fun() ->
        Q = qlc:q([X || X <- mnesia:table(icu)]),
        qlc:e(Q)
      end,
      { atomic, Records } = mnesia:transaction(Fun),

      Json = convert_to_json(lists:reverse(Records)),
      io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),

      [{status, 200},
        {header, {content_type, "text/html; charset=UTF-8"}},
        {header, {"Access-Control-Allow-Origin", "http://localhost:8000"}},
        {html, Json}
      ];

  getIcu('GET', Arg, ["api", YearA, MonthA, DayA, YearB, MonthB, DayB]) ->

    DaysA = calendar:date_to_gregorian_days(list_to_integer(YearA), list_to_integer(MonthA), list_to_integer(DayA) ),
    DaysB = calendar:date_to_gregorian_days(list_to_integer(YearB), list_to_integer(MonthB), list_to_integer(DayB) ),

      Fun = fun() ->
        Q = qlc:q([X || X <- mnesia:table(icu), days(X#icu.date) >= DaysA, DaysB >= days(X#icu.date)]),
        qlc:e(Q)
      end,
      { atomic, Records } = mnesia:transaction(Fun),

      Json = convert_to_json(lists:reverse(Records)),
      io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),

      [{status, 200},
        {header, {content_type, "text/html; charset=UTF-8"}},
        {header, {"Access-Control-Allow-Origin", "http://localhost:8000"}},
        {html, Json}
      ];

  getIcu('GET', Arg, ["api", Code]) ->

      io:format("~n ~p:~p GET Request", [?MODULE, ?LINE]),

      Fun = fun() ->
        mnesia:read({icu, list_to_integer(Code)})
      end,
      { atomic, Records } = mnesia:transaction(Fun),

      Json = convert_to_json(Records),
      io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),

      [{status, 200},
        {header, {content_type, "text/html; charset=UTF-8"}},
        {header, {"Access-Control-Allow-Origin", "http://localhost:8000"}},
        {html, Json}
      ].

  days({{Year, Month, Day}, _}) ->
    calendar:date_to_gregorian_days(Year, Month, Day ).

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
