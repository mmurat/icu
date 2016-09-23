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

%% Tab -> tablosunun tüm kayıtları

getTable(Tab) ->
  F = fun() ->
        Q = qlc:q([ I || I <- mnesia:table(Tab) ]),
        qlc:e(Q)
      end,
      mnesia:transaction(F).

provinceDropDown() ->
  { 'div', [{'class', "col-md-12"}], [
    { 'div', [{'class', "form-group"}], [
      { 'label', [{'for', "select-picker"}], <<"İl :"/utf8>> },
      { 'select', [{'class', "select-picker"}, {'id', "province"}], [

        createProvinces()
      ]}
    ]}
  ]}.

hospitalSelectPicker() ->
  { 'div', [{'class', "col-md-12"}], [
    {'div', [{'class', "form-group"}], [
      { 'label', [{'for', "select-picker"}], <<"Hastane :"/utf8>> },
      { 'select', [{'class', "select-picker"}], [
        createHospitals()
      ]}
    ]}
  ]}.


%% Yoğunbakım isteğinin kayıt formu

icuForm() ->
  {'div', [{'class', "well"}], [
    {'h3', [], <<"Yeni yoğunbakım araması"/utf8>> },
      {form, [], [

        {'div', [{'class', "form-group"}], [
          { 'input', [ {'type', "number"}, {'class', "form-control"}, {'placeholder', <<"Protokol no..."/utf8>> }, {'id', "icuCode"} ], [] }
        ]},

        {'div', [{'class', "form-group"}], [
          { 'input', [ {'type', "text"}, {'class', "form-control"}, {'placeholder', <<"Ad..."/utf8>> }, {'id', "icuName"} ], [] }
        ]},
        { 'div', [{'class', "row"}], [
          { 'div', [{'class', "col-md-3"} ], [
            { 'div', [{'class', "row"}, {'align', "right" }], [

                provinceDropDown()
            ]},
            { 'div', [{'class', "row"}, {'id', "hospital" }, {'align', "right" } ], [
                hospitalSelectPicker()
            ]}
          ]},
          {'div', [{'class', "col-md-9"}], [
          {'div', [{'class', "form-group"}], [
            { 'label', [{'class', "control-label"}], <<"Yoğunbakım "/utf8>> },
            { 'div', [{'class', "checkbox"}], [
              { 'ul', [{'class', "checkbox"}], [
                  createIcuTypes()
            ]}
          ]}
        ]}
      ]}
    ]}
    ]}
  ]}.

%% Yoğunbakım listesinin tablodan okunup kayıt formuna ekleme

createIcuTypes() ->
  { atomic, IcuTypeList } = getTable(icu_type),
  SIcuTypeList = lists:sort(IcuTypeList),
  [ createIcuType( I#icu_type.code, I#icu_type.name ) || I <- SIcuTypeList ].

createIcuType(Code, Name) ->
  {'li', [], [
    { 'label', [], [
      { 'input', [ {'type', "checkbox"}, { 'name', Code }, { 'value', Code } ]},
      Name
    ]}
  ]}.

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

out(_Arg) ->
  [
    { ehtml,
      [
        header(),
        navbar(),
        icuForm(),

        {'script', [{'type', "text/javascript"}, {'src', "js/jquery-3.1.0.min.js"}], []},
        {'script', [{'type', "text/javascript"}, {'src', "js/script.js"}], []}
      ]
    },
    { header, {content_type, erase} },
    { header, {content_type, "text/html; charset=UTF-8"} }
  ].
