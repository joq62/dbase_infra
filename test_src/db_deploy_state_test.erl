%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_deploy_state_test).   
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").
-include("controller.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
   % init 
    AllInfo=deploy_state_info_all(),
    AllInfo=lists:keysort(1,db_deploy_state:read_all()),
    
    Pods1=[{Id,"c100",node()}||Id<-db_deployment:pod_specs({"divi_multi","1.0.0"})],
    {ok,_}=db_deploy_state:create({"divi_multi","1.0.0"},Pods1),
    [{Id1,
      {"divi_multi","1.0.0"},
      [{{"mydivi_c200","1.0.0"},"c100",test@c100},
       {{"mydivi_c201","1.0.0"},"c100",test@c100},
       {{"mydivi_c202","1.0.0"},"c100",test@c100},
       {{"mydivi_c203","1.0.0"},"c100",test@c100}]}]=db_deploy_state:read_all(),
    
    Pods2=[{Id,"c203",node()}||Id<-db_deployment:pod_specs({"add_1","1.0.0"})],
    {ok,_}=db_deploy_state:create({"add_1","1.0.0"},Pods2),

    [{Id1,
      {"divi_multi","1.0.0"},
      [{{"mydivi_c200","1.0.0"},"c100",test@c100},
       {{"mydivi_c201","1.0.0"},"c100",test@c100},
       {{"mydivi_c202","1.0.0"},"c100",test@c100},
       {{"mydivi_c203","1.0.0"},"c100",test@c100}]},
     {Id2,
      {"add_1","1.0.0"},
      [{{"myadd","1.0.0"},"c203",test@c100}]}]=lists:keysort(1,db_deploy_state:read_all()),

    [Id1,Id2]=lists:sort(db_deploy_state:deploy_id()),
    {"divi_multi","1.0.0"}=db_deploy_state:deployment_id(Id1),
   
    [{Id1,
      {"divi_multi","1.0.0"},
      [{{"mydivi_c200","1.0.0"},"c100",test@c100},
       {{"mydivi_c201","1.0.0"},"c100",test@c100},
       {{"mydivi_c202","1.0.0"},"c100",test@c100},
       {{"mydivi_c203","1.0.0"},"c100",test@c100}]}]=db_deploy_state:deployment({"divi_multi","1.0.0"}),
    
    [{{"mydivi_c200","1.0.0"},"c100",test@c100},
     {{"mydivi_c201","1.0.0"},"c100",test@c100},
     {{"mydivi_c202","1.0.0"},"c100",test@c100},
     {{"mydivi_c203","1.0.0"},"c100",test@c100}]=db_deploy_state:pods(Id1),
    
    {error,[eexist,{"myadd","1.0.0"}]}=db_deploy_state:pod_node(Id1,{"myadd","1.0.0"}),
    test@c100=db_deploy_state:pod_node(Id2,{"myadd","1.0.0"}),
    {atomic,ok}=db_deploy_state:add_pod_status(Id1,{{"mydivi_c202","1.0.0"},"c201",host2@c201}),
    
    [{Id1,
      {"divi_multi","1.0.0"},
      [{{"mydivi_c202","1.0.0"},"c201",host2@c201},
       {{"mydivi_c200","1.0.0"},"c100",test@c100},
       {{"mydivi_c201","1.0.0"},"c100",test@c100},
       {{"mydivi_c203","1.0.0"},"c100",test@c100}]}]=db_deploy_state:deployment({"divi_multi","1.0.0"}),
    
    
    


    ok. 

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

deploy_state_info_all()->
    
    A=[],
    lists:keysort(1,A).
