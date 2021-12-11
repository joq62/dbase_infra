%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_deployment_test).   
    
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
    AllInfo=deployment_info_all(),
    AllInfo=lists:keysort(1,db_deployment:read_all()),
    ["math_1","add_1","divi_1","divi_multi"]=db_deployment:name(),
    
    "add_1"=db_deployment:name({"add_1","1.0.0"}),
    "1.0.0"=db_deployment:vsn({"add_1","1.0.0"}),
    [{"myadd","1.0.0"}]=db_deployment:pod_specs({"add_1","1.0.0"}),
   [{"mydivi_c200","1.0.0"},
    {"mydivi_c201","1.0.0"},
    {"mydivi_c202","1.0.0"},
    {"mydivi_c203","1.0.0"}]=db_deployment:pod_specs({"divi_multi","1.0.0"}),
    
    
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

deployment_info_all()->
    
    A=[{{"add_1","1.0.0"},
	"add_1","1.0.0",
	[{"myadd","1.0.0"}],
	stopped},
       {{"divi_1","1.0.0"},
	"divi_1","1.0.0",
	[{"mydivi","1.0.0"}],
	stopped},
       {{"divi_multi","1.0.0"},
	"divi_multi","1.0.0",
	[{"mydivi_c200","1.0.0"},
	 {"mydivi_c201","1.0.0"},
	 {"mydivi_c202","1.0.0"},
	 {"mydivi_c203","1.0.0"}],
	stopped}, 
       {{"math_1","1.0.0"},
	"math_1","1.0.0",
	[{"single_mymath","1.0.0"},
	 {"mydivi","1.0.0"},
	 {"myadd","1.0.0"}],
	stopped}],
    lists:keysort(1,A).
