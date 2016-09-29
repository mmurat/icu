-module(rest).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").

-export([out/1]).

out(Arg) ->
  Method = method(Arg),
  io:format("~p: ~p ~p Request ~n", [?MODULE, ?LINE, Method]),
  handle(Method, Arg).

method(Arg) ->
  Rec = Arg#arg.req,
  Rec#http_request.method.


  formatDate( {_Date={Year,Month,Day},_Time={Hour,Minutes, _Seconds}}) ->
    list_to_bitstring(integer_to_list(Day) ++ "/" ++ integer_to_list(Month) ++ "/" ++
    integer_to_list(Year) ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minutes)).

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

  handle('GET', _Arg) ->
    io:format("~n ~p:~p GET Request ~n", [?MODULE, ?LINE]),
    Fun = fun() ->
      Q = qlc:q([X || X <- mnesia:table(icu)]),
      qlc:e(Q)
    end,
    { atomic, Records } = mnesia:transaction(Fun),

    Json = convert_to_json(Records),
    io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),
    {html, Json};

  handle('POST', Arg) ->

    {ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
    io:format("~n~p:~p POST request ~p~n", [?MODULE, ?LINE, Json]),

     Code        = rfc4627:get_field(Json, "ncode", <<>>),
     Name        = rfc4627:get_field(Json, "name", <<>>),
     Province    = rfc4627:get_field(Json, "province", <<>>),
     Hospital    = rfc4627:get_field(Json, "hospital", <<>>),
     Insurance   = rfc4627:get_field(Json, "insurance", <<>>),
     IcuType     = rfc4627:get_field(Json, "icu_type", <<>>),
     Success     = rfc4627:get_field(Json, "success", <<>>),
     User        = rfc4627:get_field(Json, "user", <<>>),
%%    Date        = rfc4627:get_field(Json, "date", <<>>),

    io:format("~n~p:~p POST request ~p~n", [Code, Name, Province]),

    _Status     = addIcu(Code, Name, Province, Hospital, Insurance, IcuType,
                              Success, User ),

    [{status, 201},
      {html, Arg#arg.clidata},
      {header, {content_type, erase}},
      {header, {content_type, "text/html; charset=UTF-8"}}
      ];

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
