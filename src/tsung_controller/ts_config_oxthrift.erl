-module(ts_config_oxthrift).

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_oxthrift.hrl").
-include("ts_config.hrl").
-include("freq_service_types.hrl").

%% DW:
-include("ts_static_data_types.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);

% DW: Retrieve recorded Thrift requests
parse_config(Element  = #xmlElement{name = oxthrift_ads_req}, Config) ->
    parse_thrift_request(Element, oxthrift_ads_req, Config);

parse_config(Element  = #xmlElement{name = oxthrift_mds_req}, Config) ->
    parse_thrift_request(Element, oxthrift_mds_req, Config);

parse_config(Element  = #xmlElement{name = oxthrift_mops_req}, Config) ->
    parse_thrift_request(Element, oxthrift_mops_req, Config);

% DW:
%parse_config(Element = #xmlElement{name=oxthrift},
parse_config(Element  = #xmlElement{name    = oxthrift_freq_req},
             Config   = #config{curid       = Id, 
                                session_tab = Tab,
                                sessions    = [CurS | _], 
                                dynvar      = DynVar,
			                          subst       = SubstFlag, 
                                match       = MatchRegExp}) ->
    %% DW: no_ack instead of parse to ignore replied message from the socket.
    {Ack,Request} =
        case ts_config:getAttr(atom, Element#xmlElement.attributes, type) of 
                  close ->
                      ?LOGF("Got Close query~p~n",[], ?NOTICE),
                      {no_ack,#oxthrift_request{fct= close}};
                  count_all ->
                      Frequency_range = parse_frequency_range(Element#xmlElement.content, []),
                      {no_ack,#oxthrift_request{fct=count_all, args=[Frequency_range]}};
                  count ->
                      Frequency_range = parse_frequency_range(Element#xmlElement.content, []),
                      {no_ack,#oxthrift_request{fct=count, args=[Frequency_range]}};
                  delete_all ->
                      Frequency_id = parse_frequency_id(Element#xmlElement.content, []),
                      {no_ack,#oxthrift_request{fct=delete_all, args=[Frequency_id]}};
                  delete ->
                      Frequency_id = parse_frequency_id(Element#xmlElement.content, []),
                      {no_ack,#oxthrift_request{fct=delete, args=Frequency_id}};
                  increment_all ->
                      Frequency_id = parse_frequency_id(Element#xmlElement.content, []),
                      {no_ack,#oxthrift_request{fct=increment_all, args=[Frequency_id]}};
                  increment ->
                      Frequency_id = parse_frequency_id(Element#xmlElement.content, []),
                      {no_ack,#oxthrift_request{fct=increment, args=Frequency_id}}
              end,
      Msg= #ts_request{ack     = Ack,
                       endpage = true,
                       dynvar_specs  = DynVar,
                       subst   = SubstFlag,
                       match   = MatchRegExp,
                       param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

%% Parsing frequency_id elements
parse_frequency_id([], Frequency_ids) ->
    Frequency_ids;
parse_frequency_id([Element = #xmlElement{name=frequency_id} | Tail], Frequency_ids) ->
    IntCnt   = ts_config:getAttr(integer, Element#xmlElement.attributes, intCnt),
    IntMins = ts_config:getAttr(integer, Element#xmlElement.attributes, intMins),
    IntBits = ts_config:getAttr(integer, Element#xmlElement.attributes, intBits),
    Key= ts_config:getAttr(string,Element#xmlElement.attributes,key,""),
    parse_frequency_id(Tail,[#freqId{key=Key,type=#freqType{interval_cnt=IntCnt,interval_mins=IntMins,interval_bits=IntBits}}|Frequency_ids]);
parse_frequency_id([_| Tail], Frequency_ids) ->
    parse_frequency_id(Tail, Frequency_ids).


%% Parsing frequency_range elements
parse_frequency_range([], Frequency_ranges) ->
    Frequency_ranges;
parse_frequency_range([Element = #xmlElement{name=frequency_range} | Tail], Frequency_ranges) ->
    IntCnt   = ts_config:getAttr(integer, Element#xmlElement.attributes, intCnt),
    IntMins = ts_config:getAttr(integer, Element#xmlElement.attributes, intMins),
    IntBits = ts_config:getAttr(integer, Element#xmlElement.attributes, intBits),
    Key= ts_config:getAttr(string,Element#xmlElement.attributes,key,""),
    Duration= ts_config:getAttr(integer,Element#xmlElement.attributes,duration,""),
%    parse_frequency_range(Tail,[#frequency_range{key=Key,intCnt=IntCnt,intMins=IntMins,intBits=IntBits,duration=Duration}|Frequency_ranges]);
    parse_frequency_range(Tail,[#freqCountRange{freq_id=#freqId{key=Key,type=#freqType{interval_cnt=IntCnt,interval_mins=IntMins,interval_bits=IntBits}},range_minutes=Duration}|Frequency_ranges]);
parse_frequency_range([_| Tail], Frequency_ranges) ->
    parse_frequency_range(Tail, Frequency_ranges).



%% parse 3 types of requests: oxthrift_ads_req, oxthrift_mds_req,
%% oxthrift_mops_req
parse_thrift_request(Element, RequestType,
                     Config   = #config{curid       = Id, 
                                        session_tab = Tab,
                                        sessions    = [CurS | _], 
                                        dynvar      = DynVar,
                                        subst       = SubstFlag, 
                                        match       = MatchRegExp}) ->
%    io:format("parse_config: name=oxthrift_gw_req check point 0.~n",[]),
    AdType = ts_config:getAttr(atom, Element#xmlElement.attributes, adtype),
    Fun = ts_config:getAttr(atom, Element#xmlElement.attributes, fct),
%    io:format(
%    "parse_config: name=oxthrift_gw_req Looking up AdType=~p,Fun=~p.~n",
%        [AdType, Fun]),

    %% Server names are defined in tsung_controller/ts_controller_sup.erl
    ServerName = 
      case RequestType of
        oxthrift_ads_req -> ts_proxy_gw_ads;
        oxthrift_mds_req -> ts_proxy_ads_mds;
        oxthrift_mops_req -> ts_proxy_mds_mops
      end,
    ArgsTuples = ts_static_data:lookup(ServerName, adtype_fct, AdType, Fun),
    %io:format("parse_config: name=oxthrift_gw_req LookUpRes=~p.~n",[LookUpRes]),
    %io:format("parse_config: name=oxthrift_gw_req check point 0.1.~n",[]),
    %io:format("parse_config: name=oxthrift_gw_req LookUpRes=~p.~n",[LookUpRes]),
    %Args = tuple_to_list(LookUpRes#fun_call.args),
    %io:format("Args is list? ~p~n", [is_list(Args)]),

    %io:format("parse_config: name=oxthrift_gw_req args=~p.~n",[Args]),
    %LookUpRes = 
    %    case ets:lookup(?GW_REQ_TAB,{AdType, Fun}) of
    %        [] ->
    %            ?LOGF("~p:parse_config LINE ~p - empty lookup~n",
    %            [?MODULE, ?LINE],?WARN);
    %        Res ->
    %           Res
    %    end,
    ArgsList = tuple_to_list(ArgsTuples),
        
    %#fun_call{args=Args} = LookUpRes,
    %DeliveryRequest = #ox_delivery_request{args = Args, fct=Fun},
    Request = #oxthrift_request{fct         = Fun,
                                args        = ArgsList},
    %io:format("parse_config: name=oxthrift_gw_req check point 1.~n",[]),
          
    Ack = no_ack,
    Msg= #ts_request{ack     = Ack,
                     endpage = true,
                     dynvar_specs  = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content).

