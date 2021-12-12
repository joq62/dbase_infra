%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(app_start_test).    
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
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
    ?debugMsg("Start setup"),
    ?assertEqual(ok,setup()),
    ?debugMsg("stop setup"),

 %   ?debugMsg("Start testXXX"),
 %   ?assertEqual(ok,single_node()),
 %   ?debugMsg("stop single_node"),
    
      %% End application tests
    ?debugMsg("Start cleanup"),
    ?assertEqual(ok,cleanup()),
    ?debugMsg("Stop cleanup"),

    ?debugMsg("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->
    {TestDir,TestPath}=?TestConfig,
    {Dir,Path}=?Config,
    os:cmd("rm -rf "++TestDir),
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++TestPath),
    os:cmd("git clone "++Path),

    %%--- Mnesia start
    application:start(dbase_infra),
    DbaseSpecs=dbase_infra:get_dbase_specs(),
    [{db_host,ok},
     {db_service_catalog,ok},
     {db_deployment,ok},
     {db_pods,ok},
     {db_deploy_state,ok},
     {db_logger,ok}]=[{Module,dbase_infra:load_from_file(Module,Dir,Directive)}||{Module,Dir,Directive}<-DbaseSpecs],

   
    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
  %  init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
