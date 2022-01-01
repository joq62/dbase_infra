-module(db_service_catalog).
-import(lists, [foreach/2]).
%-compile(export_all).
-export([
	 vsn/1,
	 app/1,
	 git_path/1
	 
	]).

-export([
	 data_from_file/1,
	 create_table/0,
	 delete_table_copy/1,
	 create/1,
	 add_table/1,
	 add_table/2,
	 add_node/3,
	 read_all_record/0,
	 read_all/0,
	 read_record/1,
	 read/1,
	 delete/1	 
	]).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,service_catalog).
-define(RECORD,service_catalog). 
-record(service_catalog,
	{
	 id,
	 app,
	 vsn,
	 git_path
	}).

%%------------------------- Application specific commands ----------------
vsn(Id)->
    Record=read_record(Id),
    Record#?RECORD.vsn.

app(Id)->
    Record=read_record(Id),
    Record#?RECORD.app.

git_path(Id)->
    Record=read_record(Id),
    Record#?RECORD.git_path.
    
%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create({Id,App,Vsn,GitPath}) ->
%   io:format("create ~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    F = fun() ->
		Record=#?RECORD{
				id=Id,
				app=App,
				vsn=Vsn,
				git_path=GitPath
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
		   [{App,Vsn,GitPath}||
		       {?RECORD,_Id,App,Vsn,GitPath}<-Z]
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
		   [R]=[{App,Vsn,GitPath}||
			   {?RECORD,_Id,App,Vsn,GitPath}<-Z],
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

data_from_file(File)->
    {ok,I}=file:consult(File),
    data(I).
data(ServiceInfo)->
    data(ServiceInfo,[]).
data([],List)->
   % io:format("List ~p~n",[List]),
    List;
data([{App,Vsn,GitPath}|T],Acc)->
   % io:format("~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    NewAcc=[{{App,Vsn},App,Vsn,GitPath}|Acc],
    data(T,NewAcc).

