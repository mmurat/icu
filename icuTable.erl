-module(icuTable).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("icu.hrl").
-import(yaws_api, [f/2]).
-exoprt([icuTable/0]).

getIcuTable() ->

  F = fun() ->
        Q = qlc:q([ I || I <- mnesia:table(Tab) ]),
        qlc:e(Q)
      end,
      mnesia:transaction(F).


icuTable() ->
  {div, ['class', "well"], [
    {'table', [{'class', "table table-bordered table-hover"}], [


      <tbody> <tr> <td><h1>h1. Bootstrap heading</h1></td> <td class=type-info>Semibold 36px</td> </tr> <tr> <td><h2>h2. Bootstrap heading</h2></td> <td class=type-info>Semibold 30px</td> </tr> <tr> <td><h3>h3. Bootstrap heading</h3></td> <td class=type-info>Semibold 24px</td> </tr> <tr> <td><h4>h4. Bootstrap heading</h4></td> <td class=type-info>Semibold 18px</td> </tr> <tr> <td><h5>h5. Bootstrap heading</h5></td> <td class=type-info>Semibold 14px</td> </tr> <tr> <td><h6>h6. Bootstrap heading</h6></td> <td class=type-info>Semibold 12px</td> </tr> </tbody>]}
  ]}
