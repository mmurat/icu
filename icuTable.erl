-module(icuTable).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").
-import(yaws_api, [f/2]).
-exoprt([icuTable/0]).

% getIcuTable(Table) ->
%
%   F = fun() ->
%         Q = qlc:q([ I || I <- mnesia:table(Table) ]),
%         qlc:e(Q)
%       end,
%       mnesia:transaction(F).

get(Table, Code) ->
  Fun = fun() -> mnesia:read(Table, Code) end,
  {atomic, [Return | _ ] } = mnesia:transaction(Fun),
  Return#Table.name.

createIcuTableRow() ->
  IcuList = getIcuTable(icu),
  [{'tr'}, [], [
    {'td', [], I#icu.code },
    {'td', [], I#icu.name },
    {'td', [], get(province, I#icu.province) },
    {'td', [], get(hospital, I#icu.hospital) },
    {'td', [], get(icu, I#icu.icu_type) },
    {'td', [], get(insurance, I#icu.insurance) },
    {'td', [], get(hospital, I#icu.success) },
    {'td', [], I#icu.date },
    {'td', [], get(user, I#icu.user)} ]  || I <- IcuList ].

icuTable() ->
  {div, ['class', "well"], [
    {'table', [{'class', "table table-bordered table-hover"}], [
      {'thead', [], [
        {'tr', [], [
          {'th', [], [
            <<"Kayıt No"/utf8>>
          ]},
          {'th', [], [
            <<"Ad"/utf8>>
          ]},
          {'th', [], [
            <<"İl"/utf8>>
          ]},
          {'th', [], [
            <<"Hastane"/utf8>>
          ]},
          {'th', [], [
            <<"Yoğunbakım"/utf8>>
          ]},
          {'th', [], [
            <<"Güvence"/utf8>>
          ]},
          {'th', [], [
            <<"Yerleşti"/utf8>>
          ]},
          {'th', [], [
            <<"Tarih"/utf8>>
          ]},
          {'th', [], [
            <<"Kullanıcı"/utf8>>
          ]}
        ]}
        {'tbody', [], [
            createIcuTableRow()
        ]}
      ]}
  ]}
  }.
