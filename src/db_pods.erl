-module(db_pods).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,pods).
-define(RECORD,pods). 


-record(pods,
	{
	 id,
	 name,
	 vsn,
	 application,
	 needs

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
ids()->
    AllRecords=read_all_record(),
    [Id||Id<-[X#?RECORD.id||X<-AllRecords]].


application(Id)->
    Record=read_record(Id),
    Record#?RECORD.application.

needs(Id)->
    Record=read_record(Id),
    Record#?RECORD.needs.

    
%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create({Id,Name,Vsn,Application,Needs}) ->
    F = fun() ->
		Record=#?RECORD{
				id=Id,
				name=Name,
				vsn=Vsn,
				application=Application,
				needs=Needs
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
		   [{Id,Name,Vsn,Application,Needs}||
		       {?RECORD,Id,Name,Vsn,Application,Needs}<-Z]
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
		   [R]=[{Id,Name,Vsn,Application,Needs}||
			   {?RECORD,Id,Name,Vsn,Application,Needs}<-Z],
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
-define(Extension,".pod_spec").
% {name,"mydivi"}.
% {vsn,"1.0.0"}.
% {info,[{mydivi,"1.0.0"}]}.
% {needs,{preffered,["c203"]}}.

data_from_file(Dir)->
    {ok,Files}=file:list_dir(Dir),
    DataFiles=[File||File<-Files,
		     ?Extension=:=filename:extension(File)],
    DataFileNames=[filename:join(Dir,File)||File<-DataFiles],
    data(DataFileNames).
    

data(DataFileNames)->
    data(DataFileNames,[]).
data([],List)->
   % io:format("List ~p~n",[List]),
    List;
data([File|T],Acc)->
    {ok,I}=file:consult(File),
    Name=proplists:get_value(name,I),
    Vsn=proplists:get_value(vsn,I),
    Id={Name,Vsn},
    Application=proplists:get_value(application,I),
    Needs=proplists:get_value(needs,I),
    NewAcc=[{Id,Name,Vsn,Application,Needs}|Acc],
    data(T,NewAcc).
