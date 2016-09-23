
%% -*- coding: utf-8 -*-

%% -- entities 

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

-record(icu, {
	code,
	date,
	name
}).

%% -- relationships

-record(icu_province, {
		icu,
		province = 1
}).

-record(icu_hospital, {
	icu,
	hospital			
}).

-record(icu_insurance, {
	icu,
	insurance
}).

-record(icu_search, {
	icu,
	icu_types = [],
	icu_hospital,
	date,
	user
	}).