-ifndef(_proxy_types_included).
-define(_proxy_types_included, yeah).

% #fun_call.fa will be the key for ets:lookup
% since it uniquely determines the function call.
-record(fun_call, 
        {adtype :: atom(),
         fct,    % Function name (requestAds, recordEvent, getOpportunity)
         args,   % full args for making original request.
                 % some fields (ex. timestamp, trax.id) might need to be
                 % removed from args for ets look up to work properly
         resp}).

-endif.
