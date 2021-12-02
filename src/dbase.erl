%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
dynamic_db_init([])->
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    ok;

dynamic_db_init([DbaseNode|T])->
    Added=node(),
    StorageType=ram_copies,
    case rpc:call(DbaseNode,mnesia,change_config,[extra_db_nodes, [Added]],5000) of
	{ok,[Added]}->
	    mnesia:add_table_copy(schema, Added,StorageType),
	    Tables=mnesia:system_info(tables),
	    mnesia:wait_for_tables(Tables,20*1000);
	Reason ->
	    io:format("Error~p~n",[{error,Reason,DbaseNode,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
	    dynamic_db_init(T) 
    end.

dynamic_load_table(Module)->
    Added=node(),
    StorageType=ram_copies,
    Module:add_table(Added,StorageType),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,20*1000).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
