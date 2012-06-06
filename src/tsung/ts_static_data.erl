%%%-------------------------------------------------------------------
%%% File    : ts_static_data.erl
%%% Author  : Dai Wei <dai.wei@openx.com>
%%% Description :
%%%     Read in a file containing Thrift requests and save it to ets 
%%%     table.
%%%
%%% Created :  27 April 2012 by Dai Wei <dai.wei@openx.com>
%%%-------------------------------------------------------------------
-module(ts_static_data).

-include("ts_static_data_types.hrl").
-include("ts_profile.hrl").


%% A lowly hack to let ets table key on fun_args pair so that we can 
%% use ets:lookup, much faster than, ets:match.
-record(fun_call_wrapper, 
        {fun_args,  % {fct, args}
         fun_call = #fun_call{}}).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, stop/1, lookup/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%-record(state, {}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

%% RecFile contains the erlang terms for thrift request.
%% Read RecFile to ets table TableName
start_link(ServerName, RecFile) -> 
  start_link(ServerName, RecFile, {undefined, undefined}).

%% Proxy:TrimFun/1, if defined, will be used to process 
%% #fun_call{}.
start_link(ServerName, RecFile, {Proxy, TrimFun}) 
  when is_atom(ServerName) ->
    InitOpts = [RecFile, Proxy, TrimFun],
    GenServOpts = [],
    gen_server:start_link({local, ServerName}, ?MODULE, InitOpts, 
        GenServOpts).

stop(ServerName) ->
    gen_server:call(ServerName, stop).

% Return the query Thrift arguments.
lookup(ServerName, adtype_fct, AdType, Fun) ->
    gen_server:call(ServerName, {lookup, adtype_fct, AdType, Fun});

% Return the Thrift response (reduce message being passed).
lookup(ServerName, fun_args, Fun, Args) ->
    gen_server:call(ServerName, {lookup, fun_args, Fun, Args}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([FileName, Proxy, TrimFun]) ->
    % Need to trigger terminate() to close file.
    process_flag(trap_exit, true),

    %io:format("entering ts_static_data:init. filename = ~p~n", [FileName]),

    Tid = ets:new(dummy_table_name, 
            [set, %named_table,
            {keypos, #fun_call_wrapper.fun_args}, % Primary key is fun_args pair.
            {read_concurrency, true}]),

    %io:format("opened ets table.~n"),
    
    read_to_ets(FileName, Tid, Proxy, TrimFun),

    ?DebugF("~p:init is finished.~n", [?MODULE]),
    {ok, Tid}.


%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

% Return the Thrift response (including {reply, ...}).
handle_call({lookup, fun_args, Fun, TrimmedArgs}, _From, Tid) ->
    LookUpRes = 
      case ets:lookup(Tid, {Fun, TrimmedArgs}) of
        [] ->
            ?LOGF("No matching key in ets. Fun = ~p." ++ 
                          "See trimmed_args.log for the lookup argument.",
                          [Fun], ?ERR),
            %% open with [write] without read truncate any existing file.
            erlterm2file:start_link(trimmed_args_server, 
              "trimmed_args.log", [write]),
            erlterm2file:log(trimmed_args_server, TrimmedArgs),
            erlterm2file:stop(trimmed_args_server),
            {error, keynotfound};
        [Res] ->
            Res
    end,
    {reply, LookUpRes#fun_call_wrapper.fun_call#fun_call.resp, Tid};

% Return the Thrift (untrimmed) argument
handle_call({lookup, adtype_fct, AdType, Fun}, _From, Tid) ->
    LookUpRes = ets:match_object(Tid, 
                  #fun_call_wrapper{
                    fun_call=#fun_call{adtype=AdType, fct=Fun, _='_'}, 
                    _='_'}),
    if 
        LookUpRes =:= [] ->
            ?LOGF("No matching key in ets. AdType = ~p, Fun = ~p",
                         [AdType, Fun], ?ERR);
        true ->
            ?DebugF("Matched key in ets!! AdType = ~p, Fun = ~p",
                         [AdType, Fun]),
            ok
    end,
    Res = lists:nth(1, LookUpRes),
    {reply, Res#fun_call_wrapper.fun_call#fun_call.args, Tid};

handle_call(_Request, _From, Tid) ->
    Reply = ok,
    {reply, Reply, Tid}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%handle_cast(stop, State) ->
%  {stop, normal, State};
handle_cast(_Msg, Tid) ->
    {noreply, Tid}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, Tid) ->
    {noreply, Tid}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(normal, state) ->
    io:format("~p:terminate~n",[?MODULE]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, Tid, _Extra) ->
    {ok, Tid}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
read_to_ets(FileName, Tid, Proxy, TrimFun) ->
    {ok, FunCalls} = file:consult(FileName),
    read_one(FunCalls, Tid, Proxy, TrimFun).

read_one([], _Tid, _Proxy, _TrimFun) ->
    ok;

read_one([FunCall = #fun_call{fct=Fun, args=Args} | T], 
          Tid, Proxy, TrimFun) ->
    % Trim/not trim Args
    NewArgs = 
      case Proxy of
        undefined -> Args;
        _Else     -> Proxy:TrimFun(Fun, Args)
      end,

    WrappedFunCall = 
      #fun_call_wrapper{fun_args={Fun, NewArgs}, fun_call=FunCall},
    ets:insert(Tid, WrappedFunCall),

    read_one(T, Tid, Proxy, TrimFun).

