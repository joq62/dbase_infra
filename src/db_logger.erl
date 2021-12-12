
-module(db_logger).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,logger_info).
-define(RECORD,logger_info). 


-record(logger_info,
	{
	 id,        % erlang:system_time(microsecond)
	 date,      % date()
	 time,      % time()
	 node,      % node()
	 severity,  % alert, ticket, info
	 msg,       % free text
	 module,    % ?MODULE
	 function,  % ?FUNCTION_NAME
	 line,      % ?LINE
	 args,      % [erlang terms]
	 status     % new, read, cleared
	}).

%%------------------------- Application specific commands ----------------
nice_print(Info)->
    {Id,{Y,M,D},{H,Min,Sec},Node,Severity,Msg,Module,Function,Line,Args,Status}=Info,
    Y1=integer_to_list(Y),
    M1=integer_to_list(M),
    D1=integer_to_list(D),
    H1=integer_to_list(H),
    Min1=integer_to_list(Min),
    S1=integer_to_list(Sec),
    Node1=atom_to_list(Node),
    Severity1=atom_to_list(Severity),
    Module1=atom_to_list(Module),
    Function1=atom_to_list(Function),
    Line1=integer_to_list(Line),
    Status1=atom_to_list(Status),
    
    DateTime=Y1++"-"++M1++"-"++D1++" "++H1++":"++Min1++":"++S1++" ",
    NodeSeverity=Node1++" "++Severity1++" ",
    MFL="{"++Module1++","++Function1++","++Line1++"} ",
    io:format("~s ~s ~s ~s ~w ~s~n",[DateTime,NodeSeverity,Msg,MFL,Args,Status1]),
    ok.


%%------------------------- Generic  dbase commands ----------------------
create_table()->
    {atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
delete_table_copy(Dest)->
    mnesia:del_table_copy(?TABLE,Dest).

create({Date,Time,Node,Severity,Msg,Module,Function,Line,Args,Status}) ->
    Id=erlang:system_time(microsecond),
    io:format(" ~p~n",[{Date,Time,Node,Severity,Msg,Module,Function,Line,Args,Status}]),
    F = fun() ->
		Record=#?RECORD{
				id=Id,        
				date=Date,
				time=Time, 
				node=Node,      
				severity=Severity,  
				msg=Msg,      
				module=Module,   
				function=Function,  
				line=Line,      
				args=Args,      
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
		   [{Id,Date,Time,Node,Severity,Msg,Module,Function,Line,Args,Status}||
		       {?RECORD,Id,Date,Time,Node,Severity,Msg,Module,Function,Line,Args,Status}<-Z]
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
