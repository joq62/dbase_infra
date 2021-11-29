-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-define(LockTimeOut, 5). %% 30 sec 

-define(TABLE,deployment).
-define(RECORD,deployment).
-record(deployment,
	{
	 name,
	 info
	}).

create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create(Name,Info) ->
    F = fun() ->
		Record=#?RECORD{
				name=Name,
				info=Info
			       },		
		mnesia:write(Record) end,
    mnesia:transaction(F).

add_table(Node,StorageType)->
    mnesia:add_table_copy(?TABLE, Node, StorageType).


add_table(StorageType)->
    mnesia:add_table_copy(?TABLE, node(), StorageType),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,20*1000).

add_node(Dest,Source,StorageType)->
    mnesia:del_table_copy(schema,Dest),
    mnesia:del_table_copy(?TABLE,Dest),
    io:format("Node~p~n",[{Dest,Source,?FUNCTION_NAME,?MODULE,?LINE}]),
    Result=case mnesia:change_config(extra_db_nodes, [Dest]) of
	       {ok,[Dest]}->
		 %  io:format("add_table_copy(schema) ~p~n",[{Dest,Source, mnesia:add_table_copy(schema,Source,StorageType),?FUNCTION_NAME,?MODULE,?LINE}]),
		   mnesia:add_table_copy(schema,Source,StorageType),
		%   io:format("add_table_copy(table) ~p~n",[{Dest,Source, mnesia:add_table_copy(?TABLE,Dest,StorageType),?FUNCTION_NAME,?MODULE,?LINE}]),
		   mnesia:add_table_copy(?TABLE, Source, StorageType),
		   Tables=mnesia:system_info(tables),
		%   io:format("Tables~p~n",[{Tables,Dest,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
		   mnesia:wait_for_tables(Tables,20*1000),
		   ok;
	       Reason ->
		   Reason
	   end,
    Result.


read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       _->
		   [{Name,Info}||{?RECORD,Name,Info}<-Z]
	   end,
    Result.

read(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.name==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       _->
		   [R]=[Info||{?RECORD,_Name,Info}<-Z],
		   R
	   end,
    Result.

delete(Object) ->

    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Object}),
			    X#?RECORD.name==Object],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).

    

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val}->
		   Val;
	       Error->
		   Error
	   end,
    Result.

%%-------------------------------------------------------------------------
%%-------------------------------------------------------------------------
-define(DeploymentSpecDirName,"deployments").
-define(DeploymentSpecPath,"https://github.com/joq62/deployments.git").


init()->
    os:cmd("rm -rf "++?DeploymentSpecDirName),
    os:cmd("git clone "++?DeploymentSpecPath),
    {ok,FileNames}=file:list_dir(?DeploymentSpecDirName),
    DeploymentFileNames=[filename:join([?DeploymentSpecDirName,FileName])||FileName<-FileNames,
								 ".deployment"==filename:extension(FileName)],
    
    
    InfoList=[file:consult(DeploymemntFileName)||DeploymemntFileName<-DeploymentFileNames],
    ok=create_table(),
    ok=init_deployment_spec(InfoList,[]),
    os:cmd("rm -rf "++?DeploymentSpecDirName),
    ok.

init_deployment_spec([],Result)->
    
    X=[{R,Reason}||{R,Reason}<-lists:append(Result),
	  R/={atomic,ok}],
    case X of
	[]->
	    ok;
	X->
	    {error,[X]}
    end;    

init_deployment_spec([{ok,Info}|T],Acc)->  
    R=do_create(Info,[]),
    init_deployment_spec(T,[R|Acc]).

do_create([],Result)->
    Result;
do_create([Record|T],Acc)->
    {Name,Info}=Record,
    R=create(Name,Info), 
    do_create(T,[{R,Record}|Acc]).
