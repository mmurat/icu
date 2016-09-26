-module(workapp).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").
-import(yaws_api, [f/2]).

-export([out/1]).

%% bootstrap ve style.css

header() ->

  {'head', [], [
    {'link', [{'rel', "stylesheet"}, {'href', "css/bootstrap.min.css"}]},
    {'link', [{'rel', "stylesheet"}, {'href', "css/style.css"}]}
  ]}.

%% Table -> tablosunun tüm kayıtları

getTable(Table) ->
  Fun = fun() ->
        Q = qlc:q([ I || I <- mnesia:table(Table) ]),
        qlc:e(Q)
      end,
      mnesia:transaction(Fun).

%% Table dan Code index' li kayıt

getRecord(Table, Code) ->
  Fun = fun() -> mnesia:read(Table, Code) end,
  {atomic, [Return | _ ] } = mnesia:transaction(Fun),

  case Table of
    province ->
      Return#province.name;
    hospital ->
      Return#hospital.name;
    icu_type ->
      Return#icu_type.name;
    insurance ->
      Return#insurance.name;
    user ->
      Return#user.name
    end.

%% Yoğunbakim tablosunun listesi

icuTable() ->
  {'div', ['class', "well"], [
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
      ]},
      {'tbody', [],
          createIcuTableRow()
      }
      ]}
      ]}.

  createIcuTableRow() ->
    IcuList = getTable(icu),
    [{'tr', [], [
      {'td', [], I#icu.code },
      {'td', [], I#icu.name },
      {'td', [], getRecord(province, I#icu.province) },
      {'td', [], getRecord(hospital, I#icu.hospital) },
      {'td', [], getRecord(icu, I#icu.icu_type) },
      {'td', [], getRecord(insurance, I#icu.insurance) },
      {'td', [], getRecord(hospital, I#icu.success) },
      {'td', [], I#icu.date },
      {'td', [], getRecord(user, I#icu.user)} ] } || I <- IcuList ].

%% Yoğunbakım isteğinin kayıt formu

icuForm() ->
  {'div', [{'class', "well"}], [
    {'h3', [], <<"Yeni yoğunbakım araması"/utf8>> },
    {'br', [], [] },
      {form, [{'action', "/"}, {'method', "post"}], [

        {'div', [{'class', "form-group"}], [
          { 'input', [ {'name', "icuCode"}, {'type', "number"}, {'class', "form-control"}, {'placeholder', <<"Protokol no..."/utf8>> }, {'id', "icuCode"} ], [] }
        ]},

        {'div', [{'class', "form-group"}], [
          { 'input', [ {'name', "icuName"}, {'type', "text"}, {'class', "form-control"}, {'placeholder', <<"Ad..."/utf8>> }, {'id', "icuName"} ], [] }
        ]},

        { 'div', [{'class', "form-group"}], [
%%          { 'label', [{'for', "province"}], <<"İl"/utf8>> },
          { 'select', [ {'name', "icuProvince"}, {'class', "select-picker"}, {'id', "province"}, {'class', "form-control"}], [
            createProvinces()
          ]}
        ]},

        { 'div', [{'class', "form-group"}], [
%%          { 'label', [{'for', "hospital"}], <<"Hastane"/utf8>> },
          { 'select', [ {'name', "icuHospital"}, {'class', "select-picker"}, {'id', "hospital"}, {'class', "form-control"}], [
            createHospitals()
          ]}
        ]},

        {'div', [{'class', "form-group"}], [
%%          { 'label', [{'for', "icu"}], <<"Yoğunbakım"/utf8>> },
          { 'select', [ {'name', "icuICU"}, {'class', "select-picker"}, {'id', "icu"}, {'class', "form-control"}], [
            createIcuTypes()
          ]}
        ]},

        {'div', [{'class', "form-group"}], [
%%          { 'label', [{'for', "insurance"}], <<"Güvence"/utf8>> },
          { 'select', [ {'name', "icuInsurance"}, {'class', "select-picker"}, {'id', "insurance"}, {'class', "form-control"}], [
            createInsurances()
          ]}
        ]},

      {'div', [{'class', "form-group"}], [
        { 'button', [{'class', "btn btn-lg btn-primary btn-block"},
        {'type', "submit"},
        {'id', "newICU"}], <<"Kayıt"/utf8>>}
      ]}
      ]}
  ]}.

%% Yoğunbakım listesinin tablodan okunup kayıt formuna ekleme

createInsurances() ->
  { atomic, InsuranceList } = getTable(insurance),
  SInsuranceList = lists:sort(InsuranceList),
  [ createInsurance(I#insurance.code, I#insurance.name ) || I <- SInsuranceList ].

createInsurance(Code, Name) ->
  { 'option', [ {'class', "form-control"}, { 'name', Code }, { 'value', Code }], Name }.

createIcuTypes() ->
  { atomic, IcuTypeList } = getTable(icu_type),
  SIcuTypeList = lists:sort(IcuTypeList),
  [ createIcuType( I#icu_type.code, I#icu_type.name ) || I <- SIcuTypeList ].

createIcuType(Code, Name) ->
      { 'option', [ {'class', "form-control"}, { 'name', Code }, { 'value', Code }], Name }.

%% İl listesinin tablodan okunup kayıt formuna ekleme

createProvinces() ->
  { atomic, ProvinceList } = getTable(province),
  SProvinceList = lists:sort(ProvinceList),
  [ createProvince( P#province.code, P#province.name ) || P <- SProvinceList ].

createProvince(Code, Name) ->
  { 'option', [ {'class', "form-control"}, { 'name', Code }, { 'value', Code } ], Name}.

%% Hastane listesinin tablodan okunup kayıt formuna ekleme

createHospitals() ->
  { atomic, HospitalList } = getTable(hospital),
  SHospitalList = lists:sort(HospitalList),
  [ createHospital( H#hospital.code, H#hospital.name ) || H <- SHospitalList ].

createHospital(Code, Name) ->
  { 'option', [ {'class', "form-control"}, { 'name', Code }, { 'value', Code } ], Name}.

%% Navbar

navbar() ->

      {'nav', [{'class', "navbar navbar-default navbar-fixed-bottom"}],
        [
          {'div', [{'class', "container-fluid"}], [
            {'div', [{'class', "navbar-header"}], [
                {'button', [{'type', "button"}, {'class', "navbar-toggle collapsed"}, {'data-toggle', "collapse"}, {'data-target', "#navbar"},
                              {'aria-expanded', "false"}, {'aria-controls', "navbar"}], [
                                {'span', [{'class', "sr-only"}], "Toggle navigation"},
                                {'span', [{'class', "icon-bar"}]},
                                {'span', [{'class', "icon-bar"}]},
                                {'span', [{'class', "icon-bar"}]}
                              ]},
                {'a', [{'class', "navbar-brand"}, {'href', "#"}], "ICU Search"}
            ]},
          {'div', [{'id', "navbar"}, {'class', "navbar-collapse collapse"}], [
            {'ul', [{'class', "nav navbar-nav navbar-right"}], [
              {'li', [], [{'a', [{'href', "#"}], "Dashboard"}]},
              {'li', [], [{'a', [{'href', "#"}], "Settings"}]},
              {'li', [], [{'a', [{'href', "#"}], "Profile"}]},
              {'li', [], [{'a', [{'href', "#"}], "Help"}]}
            ]},
            {'form', [{'class', "navbar-form navbar-right"}],
              [{'input', [{'type', "text"}, {'class', "form-control"}, {'placeholder', "Search..."}]}]}
          ]}
          ]}
        ]
      }.


createNewIcu([   {"icuCode", Code}, {"icuName", Name},
                {"icuProvince", Province}, {"icuHospital", Hospital},
                {"icuICU", IcuType}, {"icuInsurance", Insurance} ]) ->

                 I_Code = list_to_integer(Code),
                 I_Province = list_to_integer(Province),
                 I_Hospital = list_to_integer(Hospital),
                 I_IcuType = list_to_integer(IcuType),
                 I_Insurance = list_to_integer(Insurance),

                  #icu{ code = I_Code,
                          name = Name,
                          province = I_Province,
                          hospital = I_Hospital,
                          insurance = I_Insurance,
                          icu_type = I_IcuType
                          }.

handle('POST', Arg) ->
  L = yaws_api:parse_post(Arg),
  io:format("~p", [L]),

  NewIcu = createNewIcu(L),
  io:format("~p", [NewIcu]),

  case (NewIcu#icu.code > 0) of
    true ->
      Fun = fun() -> mnesia:write(NewIcu) end,
      mnesia:transaction(Fun),
      handle('GET', Arg);

    false ->
      handle('GET', Arg)
    end;

  % [{status, 201},
  % {html, Arg#arg.clidata},
  % {header, {content_type, erase}},
  % {header, {content_type, "text/html; charset=UTF-8"}}
  % ];

handle('GET', _Arg) ->

  [
    { ehtml,
      [
        header(),
        navbar(),
        icuForm(),
        icuTable(),

        {'script', [{'type', "text/javascript"}, {'src', "js/jquery-3.1.0.min.js"}], []},
        {'script', [{'type', "text/javascript"}, {'src', "js/script.js"}], []}
      ]
    },
    { header, {content_type, erase} },
    { header, {content_type, "text/html; charset=UTF-8"} }
  ].

method(Arg) ->
  Rec = Arg#arg.req,
  Rec#http_request.method.

out(Arg) ->
  Method = method(Arg),
  io:format("~p: ~p ~p Request ~n", [?MODULE, ?LINE, Method]),
  handle(Method, Arg).
