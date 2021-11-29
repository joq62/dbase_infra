%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_infra_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("").
%% --------------------------------------------------------------------

-define(ScheduleInterval,1*10*1000).

%% External exports
-export([
	 schedule/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {loaded,
		spec_list
	       }).

%% ====================================================================
%% External functions
%% ====================================================================


schedule()->
    gen_server:cast(?MODULE, {schedule}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    case bully:am_i_leader(node()) of
	false->
	    mnesia:stop(),
	    mnesia:del_table_copy(schema,node()),
	    mnesia:delete_schema([node()]),
	    mnesia:start();
	true->
	    {ok,WantedNodes}=application:get_env(dbase_infra,nodes),
	    RunningNodes=[Node||Node<-WantedNodes,
				pong=:=net_adm:ping(Node)],
	    DbaseNodes=[Node||Node<-RunningNodes,
			      yes=:=rpc:call(Node,mnesia,system_info,[is_running],1000)],
						%  io:format("node(),DbaseNodes ~p~n",[{node(),DbaseNodes,?FUNCTION_NAME,?MODULE,?LINE}]),
	    ok=dbase:dynamic_db_init(DbaseNodes),
	    ok
    end,
    
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({allocate,App},_From, State) ->
    Reply=loader:allocate(App),
    {reply, Reply, State};


handle_call({loaded},_From, State) ->
    Reply=State#state.loaded,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({deallocate,Node,App}, State) ->
    loader:deallocate(Node,App),
    {noreply, State};

handle_cast({schedule}, State) ->
     spawn(fun()->do_schedule() end),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_schedule()->
    io:format("do_schedule start~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,time()}]),
    timer:sleep(?ScheduleInterval),
 %   Result=rpc:call(node(),scheduler,start,[],10*1000),
 %   not_implmented=Result,
 %   io:format("~p~n",[{Result,?MODULE,?FUNCTION_NAME,?LINE,time()}]),
    rpc:cast(node(),controller_server,schedule,[]).
		  
