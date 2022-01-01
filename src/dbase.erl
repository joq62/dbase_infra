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
-include("controller.hrl").
%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 dynamic_db_init/1,
	 dynamic_load_table/1
	 ]).
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
    [dbase_data:load_from_file(Module,Dir,Type)||{Module,Dir,Type}<-dbase_infra:get_dbase_specs()],
    
    ok;

dynamic_db_init([DbaseNode|T])->
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
     mnesia:start(),
    io:format("DbaseNode dynamic_db_init([DbaseNode|T]) ~p~n",[{DbaseNode,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    StorageType=ram_copies,
  %  case rpc:call(DbaseNode,mnesia,change_config,[extra_db_nodes, [node()]],5000) of
    case rpc:call(node(),mnesia,change_config,[extra_db_nodes,[DbaseNode]],5000) of
	{ok,[_AddedNode]}->
	    Tables=mnesia:system_info(tables),
	    [mnesia:add_table_copy(Table, node(),StorageType)||Table<-Tables,
								       Table/=schema],
	    mnesia:wait_for_tables(Tables,20*1000);
	_Reason ->
	    dynamic_db_init(T) 
    end.

dynamic_load_table(Module)->
  %  io:format("Module ~p~n",[{Module,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
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
