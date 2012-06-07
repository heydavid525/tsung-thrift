-ifndef(_ts_oxthrift_included).
-define(_ts_oxthrift_included, yeah).

%% use by the client to create the request

-record(oxthrift_request, {
          % DW: The common Thrift request format across delivery servers.
          fct, args,     

          % DW: frequency requests uses additional info (according to HDL):
          %type,
          freq_id = [],
          freq_range = []
          %data             % HDL - need to know how this will work
        }).

%%
-record(frequency_id, {
          intCnt,
          intMins,
          intBits,
          key
         }).
%%
-record(frequency_range, {
          intCnt,
          intMins,
          intBits,
          key,
          duration
         }).
%% 
-record(oxthrift_dyndata, 
        { 
          none
         }
       ).

%% unused
-record(oxthrift, 
        { 
          fixme
         }
       ).

%%% Supported byte code instructions
-define(COUNT, 0).

-endif.
