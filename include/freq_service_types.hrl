-ifndef(_freq_service_types_included).
-define(_freq_service_types_included, yeah).

%% struct freqType

-record(freqType, {interval_cnt = undefined :: integer(), 
                   interval_mins = undefined :: integer(), 
                   interval_bits = undefined :: integer()}).

%% struct freqId

-record(freqId, {type = #freqType{} :: #freqType{}, 
                 key = undefined :: string()}).

%% struct freqCountRange

-record(freqCountRange, {freq_id = #freqId{} :: #freqId{}, 
                         range_minutes = undefined :: integer()}).

%% struct freqCount

-record(freqCount, {count = undefined :: integer(), 
                    last_update_ts = undefined :: integer()}).

%% struct freqServiceException

-record(freqServiceException, {err_msg = undefined :: string()}).

-endif.
