%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_data).   
 
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("controller.hrl").
%% --------------------------------------------------------------------


%% External exports
-export([
	 load_from_file/3,

	 load_configs/0,
	 delete_configs/0,
	 load_configs/1,
	 connect/0,
	 start_needed_apps/0,
	 initiate_dbase/0,
	 initiate_dbase/1,
	 load_services/0
	]).
    


%% ====================================================================
%% External functions
%% ====================================================================
load_from_file(Module,Dir,yes)->
    ok=rpc:call(node(),Module,create_table,[],5*1000),
    AllData=rpc:call(node(),Module,data_from_file,[Dir],5*1000),
    CreateResult=[rpc:call(node(),Module,create,[Data],5*1000)||Data<-AllData],
    Reply=case [R||R<-CreateResult,R/=ok] of
	      []->
		  ok;
	      ErrorList->
		  {error,ErrorList}
	  end,
    Reply;
load_from_file(Module,na,no)->
    Reply=rpc:call(node(),Module,create_table,[],5*1000),
    Reply.

   
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% -------------------------------------------------------------------

load_configs()->
    {TestDir,TestPath}=?TestConfig,
    {Dir,Path}=?Config,
    os:cmd("rm -rf "++TestDir),
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++TestPath),
    os:cmd("git clone "++Path),
    ok.

delete_configs()->
    {TestDir,TestPath}=?TestConfig,
    {Dir,Path}=?Config,
    os:cmd("rm -rf "++TestDir),
    os:cmd("rm -rf "++Dir),
    ok.
    

load_configs(Root)->
    {TestDir,TestPath}=?TestConfig,
    {ProductionDir,Path}=?Config,
    TDir=filename:join(Root,TestDir),
    PDir=filename:join(Root,ProductionDir),
    os:cmd("rm -rf "++TDir),
    os:cmd("rm -rf "++PDir),
    os:cmd("git clone "++TestPath),
    os:cmd("mv "++TestDir++" "++Root),
    os:cmd("git clone "++Path),
    os:cmd("mv "++ProductionDir++" "++Root),
    ok.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% -------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
connect()->
    connect:start(?ControllerNodes),
    ok.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start_needed_apps()->
    ok=application:start(dbase_infra),
 %   ok=initiate_dbase(),
    ok=application:start(sd),
    ok=application:start(logger_infra),
    ControllerNodes=connect:get(?ControllerNodes),
    application:set_env([{bully,[{nodes,ControllerNodes}]}]),
    ok=application:start(bully),
    ok=application:start(host),

    timer:sleep(1000),
    ok.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
initiate_dbase()->
    initiate_dbase(".").    
initiate_dbase(Root)->
%    ControllerNodesSpecFile=filename:join(Root,?ControllerNodes),
%    RunningNodes=lists:delete(node(),connect:start(ControllerNodesSpecFile)),
    
    RunningNodes=lists:delete(node(),sd:get(dbase_infra)),
    NodesMnesiaStarted=[Node||Node<-RunningNodes,
			      yes=:=rpc:call(Node,mnesia,system_info,[is_running],1000)],
    io:format("NodesMnesiaStarted ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,node(),NodesMnesiaStarted}]),
    DbaseSpecs=dbase_infra:get_dbase_specs(),
    io:format("DbaseSpecs ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,node(),DbaseSpecs}]),
    case NodesMnesiaStarted of
	[]-> % initial start
	    DbaseSpecs_2=[{Module,filename:join(Root,Dir),Directive}||{Module,Dir,Directive}<-DbaseSpecs],
	    LoadResult=[{Module,dbase_infra:load_from_file(Module,Dir,Directive)}||{Module,Dir,Directive}<-DbaseSpecs_2],
	    io:format("LoadResult ~p~n",[{LoadResult,?MODULE,?FUNCTION_NAME,?LINE,node()}]),
	    case [{Module,R}||{Module,R}<-LoadResult,R/=ok] of
		[]->
		    ok;
		ReasonList->
		    {error,ReasonList}
	    end;
	[Node0|_]->
	    io:format("Node0 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Node0}]),
	    ok=rpc:call(node(),dbase_infra,add_dynamic,[Node0],3*1000),
	    timer:sleep(500),
	    _R=[rpc:call(node(),dbase_infra,dynamic_load_table,[node(),Module],3*1000)||{Module,_}<-DbaseSpecs],
	    
	    timer:sleep(500),
	    ok
    end,
    ok.


init_dbase_service(Node,{Module,Source,Directive})->
    LoadResult=[R||R<-rpc:call(Node,dbase_infra,load_from_file,[Module,Source,Directive],2*1000),
			   R/=ok],
    Result=case LoadResult of
	       []-> %ok
		   {ok,[Node,Module]};
	       Reason ->
		   {error,[Node,Module,Reason]}
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
load_services()->
    EnvVar=service_catalog,
    Env=appfile:read("controller.app",env),
    {EnvVar,Info}=lists:keyfind(EnvVar,1,Env),
    Dir=proplists:get_value(dir,Info),
    FileName=proplists:get_value(filename,Info),
    GitPath=proplists:get_value(git_path,Info),
    RootDir="my_services",

    os:cmd("rm -rf "++RootDir),
    ok=file:make_dir(RootDir),
    
    ok=clone(Dir,GitPath),
    {ok,CatalogInfo}=catalog_info(Dir,FileName),
    [load_service(RootDir,ServiceInfo)||ServiceInfo<-CatalogInfo].
    

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
load_service(RootDir,{App,_Vsn,GitPath})->
    AppId=atom_to_list(App),
    SourceDir=AppId,
    DestDir=filename:join(RootDir,AppId),
    os:cmd("rm -rf "++DestDir),
    os:cmd("git clone "++GitPath),
    os:cmd("mv "++SourceDir++" "++DestDir),
    case code:add_patha(filename:join(DestDir,"ebin")) of
	true->
	    ok=application:load(App),
	    {ok,App};
	Reason->
	    {error,[Reason,App,DestDir]}
    end.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
clone(Dir,GitPath)->
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitPath),

    ok.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
catalog_info(Dir,FileName)->
    {ok,CatalogInfo}=file:consult(filename:join([Dir,FileName])),    
    {ok,CatalogInfo}.
