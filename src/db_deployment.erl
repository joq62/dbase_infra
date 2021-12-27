-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,deployment).
-define(RECORD,deployment). 


-record(deployment,
	{
	 id,
	 name,
	 vsn,
	 pod_specs,
	 affinity,
	 status
	}).

%%------------------------- Application specific commands ----------------
name()->
    AllRecords=read_all_record(),
    [I||I<-[X#?RECORD.name||X<-AllRecords]].
name(Id)->
    Record=read_record(Id),
    Record#?RECORD.name.
vsn()->
    AllRecords=read_all_record(),
    [I||I<-[X#?RECORD.vsn||X<-AllRecords]].
vsn(Id)->
    Record=read_record(Id),
    Record#?RECORD.vsn.
pod_specs(Id)->
    Record=read_record(Id),
    Record#?RECORD.pod_specs.
affinity(Id)->
    Record=read_record(Id),
    Record#?RECORD.affinity.    
    
%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create({Id,Name,Vsn,PodSpecs,Affinity,Status}) ->
%   io:format("create ~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    F = fun() ->
		Record=#?RECORD{
				id=Id,
				name=Name,
				vsn=Vsn,
				pod_specs=PodSpecs,
				affinity=Affinity,
				status=Status
			       },		
		mnesia:write(Record) end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    ok;
	ErrorReason ->
	    ErrorReason
    end.

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

read_all_record()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       _->
		   Z
	   end,
    Result.
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       _->
		   [{Id,Name,Vsn,PodSpecs,Affinity,Status}||
		       {?RECORD,Id,Name,Vsn,PodSpecs,Affinity,Status}<-Z]
	   end,
    Result.

read_record(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       [X]->
		   X
	   end,
    Result.

read(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       _->
		   [R]=[{Id,Name,Vsn,PodSpecs,Affinity,Status}||
			   {?RECORD,Id,Name,Vsn,PodSpecs,Affinity,Status}<-Z],
		   R
	   end,
    Result.

delete(Object) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Object}),
			    X#?RECORD.id==Object],
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

%%--------------------------------------------------------------------
-define(Extension,".deployment").
% {name,"mydivi"}.
% {vsn,"1.0.0"}.
% {pod_specs,[{infra,"1.0.0"}]}.

data_from_file(Dir)->
    {ok,Files}=file:list_dir(Dir),
    InfoFiles=[File||File<-Files,
		     ?Extension=:=filename:extension(File)],
    InfoFileNames=[filename:join(Dir,File)||File<-InfoFiles],
    data(InfoFileNames).
    

data(InfoFileNames)->
    data(InfoFileNames,[]).
data([],List)->
   % io:format("List ~p~n",[List]),
    List;
data([File|T],Acc)->
    {ok,I}=file:consult(File),
    Name=proplists:get_value(name,I),
    Vsn=proplists:get_value(vsn,I),
    Id={Name,Vsn},
    PodSpecs=proplists:get_value(pod_specs,I),
    Affinity=proplists:get_value(affinity,I),
    Status=stopped,
%    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Id,Name,Vsn,PodSpecs,Status}]),
    NewAcc=[{Id,Name,Vsn,PodSpecs,Affinity,Status}|Acc],
    data(T,NewAcc).
