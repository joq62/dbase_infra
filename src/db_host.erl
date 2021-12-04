-module(db_host).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,host).
-define(RECORD,host). 
-record(host,
	{
	 id,
	 access_info,
	 type,
	 start_args,
	 dirs_to_keep,
	 application_dir,
	 status
	}).

%%------------------------- Application specific commands ----------------
access_info()->   
    AllRecords=read_all_record(),
    [{X#?RECORD.id,X#?RECORD.access_info}||X<-AllRecords].
access_info(Id)->   
    Record=read_record(Id),
    Record#?RECORD.access_info.

status()->
    AllRecords=read_all_record(),
    [{X#?RECORD.id,X#?RECORD.status}||X<-AllRecords].
status(Id)->
    Record=read_record(Id),
    Record#?RECORD.status.

hosts()->
    AllRecords=read_all_record(),
    [Host||{Host,_}<-[X#?RECORD.id||X<-AllRecords]].

ids()->
    AllRecords=read_all_record(),
    [Id||Id<-[X#?RECORD.id||X<-AllRecords]].


start_args(Id)->
    Record=read_record(Id),
    Record#?RECORD.start_args.

type(Id)->
    Record=read_record(Id),
    Record#?RECORD.type.

dirs_to_keep(Id)->
    Record=read_record(Id),
    Record#?RECORD.dirs_to_keep.

application_dir(Id)->
    Record=read_record(Id),
    Record#?RECORD.application_dir.

ip(Id)->
    I=access_info(Id),
    proplists:get_value(ip,I).
port(Id)->
    I=access_info(Id),
    proplists:get_value(ssh_port,I).
uid(Id)->
    I=access_info(Id),
    proplists:get_value(uid,I).
passwd(Id)->
    I=access_info(Id),
    proplists:get_value(pwd,I).
node(Id)->
    I=access_info(Id),
    proplists:get_value(node,I).

erl_cmd(Id)->
    I=start_args(Id),
    proplists:get_value(erl_cmd,I).
env_vars(Id)->
    I=start_args(Id),
    proplists:get_value(env_vars,I).
cookie(Id)->
    I=start_args(Id),
    proplists:get_value(cookie,I).
nodename(Id)->
    I=start_args(Id),
    proplists:get_value(nodename,I).

    
%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create({Id,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}) ->
%   io:format("create ~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    F = fun() ->
		Record=#?RECORD{
				id=Id,
				access_info=AccessInfo,
				type=Type,
				start_args=StartArgs,
				dirs_to_keep=DirsToKeep,
				application_dir=AppDir,
				status=Status
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
		   [{Id,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}||
		       {?RECORD,Id,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}<-Z]
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
		   [R]=[{Id,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}||
			   {?RECORD,Id,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}<-Z],
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
update_status(Object,NewStatus)->
 F = fun() -> 
	     RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.id==Object])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     NewRecord=S1#?RECORD{status=NewStatus},
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
-define(Extension,".host").
data_from_file(Dir)->
    {ok,Files}=file:list_dir(Dir),
    HostFiles=[File||File<-Files,
		     ?Extension=:=filename:extension(File)],
    HostFileNames=[filename:join(Dir,File)||File<-HostFiles],
    data(HostFileNames).
    

data(HostFileNames)->
    data(HostFileNames,[]).
data([],List)->
   % io:format("List ~p~n",[List]),
    List;
data([HostFile|T],Acc)->
    {ok,I}=file:consult(HostFile),
    Id=proplists:get_value(id,I),
    StartArgs=proplists:get_value(start_args,I),
    AccessInfo=proplists:get_value(access_info,I),
    Type=proplists:get_value(host_type,I),
    DirsToKeep=proplists:get_value(dirs_to_keep,I),
    AppDir=proplists:get_value(application_dir,I),
    Status=stopped,
   % io:format("~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    NewAcc=[{Id,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}|Acc],
    data(T,NewAcc).

