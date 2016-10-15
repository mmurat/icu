
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
  %%getIcu('GET', Arg, ["api"]); 
    
    % [{status, 201},
    %   {html, {obj, rfc4627:encode([{data, Icu}])}},
    %   {header, {content_type, erase}},
    %   {header, {content_type, "text/html; charset=UTF-8"}}
    %   ];

  handle('PUT', Arg) ->
    [IndexValue, _]     = string:tokens(Arg#arg.pathinfo, "/"),
    {ok, Json, _}       = rfc4627:decode(Arg#arg.clidata),
    io:format("~p:~p PUT request ~p ~p~n",
      [?MODULE, ?LINE, IndexValue, Json]),

      Code        = rfc4627:get_field(Json, "code", <<>>),
      Name        = rfc4627:get_field(Json, "name", <<>>),
      Province    = rfc4627:get_field(Json, "province", <<>>),
      Hospital    = rfc4627:get_field(Json, "hospital", <<>>),
      Insurance   = rfc4627:get_field(Json, "insurance", <<>>),
      IcuType     = rfc4627:get_field(Json, "icu_type", <<>>),
      Success     = rfc4627:get_field(Json, "success", <<>>),
      User        = rfc4627:get_field(Json, "user", <<>>),
  %%    Date        = rfc4627:get_field(Json, "date", <<>>),

  NewRec =
    #icu{ code = Code,
      name = Name,
      province = Province,
      hospital = Hospital,
      insurance = Insurance,
      icu_type = IcuType,
      success = Success,
      user = User
      %%date = erlang:localtime() %%
    },

    io:format("~p:~p Renaming ~p",
      [?MODULE, ?LINE, NewRec]),

    ChangeName         = fun() ->
                            mnesia:delete({icu, IndexValue}),
                            mnesia:write(NewRec)
                          end,
    {atomic, _Rec}      = mnesia:transaction(ChangeName),
    [{status, 200},
      {html, IndexValue}];

  handle('DELETE', Arg) ->
    [IndexValue, _]     = string:tokens(Arg#arg.pathinfo,"/"),
    io:format("~p:~p DELETE request ~p",
      [?MODULE, ?LINE, IndexValue]),

    Delete              = fun() ->
      mnesia:delete({icu, IndexValue})
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

  do(Q) ->
    F = fun() ->
      qlc:e(Q)
        end,
    {atomic, Value}   = mnesia:transaction(F),
    Value.


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