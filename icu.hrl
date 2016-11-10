
%% -*- coding: utf-8 -*-

-record(user, {
	code,
	name
	}).

-record(hospital, {
	code,
	name
}).

-record(province, {
	code,
	name
}).

-record(icu_type, {
	code,
	name
}).

-record(insurance, {
	code,
	name
}).

-record(success, {
	code,
	name
}).

-record(icu, {
	code,
  name,
  province = 0,
  hospital = 0,
  insurance = 0,
	icu_type = 0,
  user = 0,
  success = 0,
  date
}).

%% -- relationships

%% -record(icu_province, {
%% icu,
%%		province = 1
%%}).

%%-record(icu_hospital, {
%%	icu,
%%	hospital
%%}).

%%-record(icu_insurance, {
%%	icu,
%%	insurance
%%}).

%%-record(icu_search, {
%%	icu,
%%	icu_type,
%%	icu_hospital,
%%	date,
%%	user
%%}).
