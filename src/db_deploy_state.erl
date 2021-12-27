-module(db_deploy_state).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,deploy_state).
-define(RECORD,deploy_state). 


-record(deploy_state,
	{
	 id, %deloyment id
	 deployment, 
	 pods   %[{PodId,Node,HostId},,,]
	}).

%%------------------------- Application specific commands ----------------

deploy_id()->
    Result=case read_all_record() of
	       {aborted,Reason}->
		   {error,Reason};
	       Records->
		   [Record#?RECORD.id||Record<-Records]
	   end,
    Result.
deployment_id(Id)->
    Result=case read_record(Id) of
	       {aborted,Reason}->
		   {error,Reason};
	       Record->
		   Record#?RECORD.deployment
	   end,
    Result.

deployment(WantedDeploymentId)->
    Result=case read_all_record() of
	       {aborted,Reason}->
		   {error,Reason};
	       Records->
		   [{Id,DeploymentId,Pods}||{?RECORD,Id,DeploymentId,Pods}<-Records,
					    WantedDeploymentId=:=DeploymentId]
	   end,
    Result.

pods(Id)->
    Result=case read_record(Id) of
	       {aborted,Reason}->
		   {error,Reason};
	       Record->
		   Record#?RECORD.pods
	   end,
    Result.

pod_node(Id,WantedPodId)->
    Result=case pods(Id) of
	       {error,Reason}->
		   {error,Reason};
	       Pods->
		   case lists:keyfind(WantedPodId,1,Pods) of
		       false->
			   {error,[eexist,WantedPodId]};
		       {_PodId,_Host,Node}->
			   Node
		   end
	   end,
    Result.
	

%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create(DeploymentId,Pods) ->
    Id=erlang:system_time(microsecond),
    F = fun() ->
		Record=#?RECORD{
				id=Id, %Unique id
				deployment=DeploymentId, 
				pods=Pods
			       },		
		mnesia:write(Record) end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    {ok,Id};
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
		   {error,Reason};
	       _->
		   Z
	   end,
    Result.
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=case Z of
	       {aborted,Reason}->
		   {error,Reason};
	       _->
		   [{Id,DeploymentId,Pods}||
		       {?RECORD,Id,DeploymentId,Pods}<-Z]
	   end,
    Result.

read_record(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {error,Reason};
	       [X]->
		   X
	   end,
    Result.

read(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {error,Reason};
	       _->
		   [R]=[{Id,DeploymentId,Pods}||
			   {?RECORD,Id,DeploymentId,Pods}<-Z],
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
add_pod_status(Object,PodInfo)->
 F = fun() -> 
	     RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.id==Object])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     %
		     {PodNode,_PodDir,_PodId}=PodInfo,		     
		     NewPods=[PodInfo|lists:keydelete(PodNode,1,S1#?RECORD.pods,PodInfo)],
		     NewRecord=S1#?RECORD{pods=NewPods},
		     mnesia:delete_object(S1),
		     mnesia:write(NewRecord)
	     end
		 
     end,
    mnesia:transaction(F).
delete_pod_status(Object,PodInfo)->
 F = fun() -> 
	     RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.id==Object])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     %
		     {PodNode,_PodDir,_PodId}=PodInfo,		     
		     NewPods=lists:keydelete(PodNode,1,S1#?RECORD.pods,PodInfo),
		     NewRecord=S1#?RECORD{pods=NewPods},
		     mnesia:delete_object(S1),
		     mnesia:write(NewRecord)
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
