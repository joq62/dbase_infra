-module(db_host).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-define(LockTimeOut, 5). %% 30 sec 

-define(TABLE,host).
-define(RECORD,host). 
-record(host,
	{
	 hostname,
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
    [{X#?RECORD.hostname,X#?RECORD.access_info}||X<-AllRecords].
access_info(Hostname)->   
    Record=read_record(Hostname),
    Record#?RECORD.access_info.
hosts()->
    AllRecords=read_all_record(),
    [X#?RECORD.hostname||X<-AllRecords].
start_args(Hostname)->
    Record=read_record(Hostname),
    Record#?RECORD.start_args.

type(Hostname)->
    Record=read_record(Hostname),
    Record#?RECORD.type.

dirs_to_keep(Hostname)->
    Record=read_record(Hostname),
    Record#?RECORD.dirs_to_keep.

application_dir(Hostname)->
    Record=read_record(Hostname),
    Record#?RECORD.application_dir.
status(Hostname)->
    Record=read_record(Hostname),
    Record#?RECORD.status.

ip(Hostname)->
    I=access_info(Hostname),
    proplists:get_value(ip,I).
port(Hostname)->
    I=access_info(Hostname),
    proplists:get_value(ssh_port,I).
uid(Hostname)->
    I=access_info(Hostname),
    proplists:get_value(uid,I).
passwd(Hostname)->
    I=access_info(Hostname),
    proplists:get_value(pwd,I).
node(Hostname)->
    I=access_info(Hostname),
    proplists:get_value(node,I).

erl_cmd(Hostname)->
    I=start_args(Hostname),
    proplists:get_value(erl_cmd,I).
env_vars(Hostname)->
    I=start_args(Hostname),
    proplists:get_value(env_vars,I).
cookie(Hostname)->
    I=start_args(Hostname),
    proplists:get_value(cookie,I).
nodename(Hostname)->
    I=start_args(Hostname),
    proplists:get_value(nodename,I).

    
%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create({HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}) ->
%   io:format("create ~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    F = fun() ->
		Record=#?RECORD{
				hostname=HostName,
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
		   [{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}||
		       {?RECORD,HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}<-Z]
	   end,
    Result.

read_record(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.hostname==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       [X]->
		   X
	   end,
    Result.

read(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.hostname==Object])),
    Result=case Z of
	       {aborted,Reason}->
		   {aborted,Reason};
	       _->
		   [R]=[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}||
			   {?RECORD,HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}<-Z],
		   R
	   end,
    Result.

delete(Object) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Object}),
			    X#?RECORD.hostname==Object],
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
				       X#?RECORD.hostname==Object])),
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
    HostName=proplists:get_value(hostname,I),
    StartArgs=proplists:get_value(start_args,I),
    AccessInfo=proplists:get_value(access_info,I),
    Type=proplists:get_value(host_type,I),
    DirsToKeep=proplists:get_value(dirs_to_keep,I),
    AppDir=proplists:get_value(application_dir,I),
    Status=stopped,
   % io:format("~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    NewAcc=[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}|Acc],
    data(T,NewAcc).

