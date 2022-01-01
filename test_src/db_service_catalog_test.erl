%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_service_catalog_test).   
    
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
    AllInfo=cata_info_all(),
    AllInfo=lists:keysort(1,db_service_catalog:read_all()),
    
    host=db_service_catalog:app({host,"1.0.0"}),
    "1.0.0"=db_service_catalog:vsn({host,"1.0.0"}),
    "https://github.com/joq62/host.git"=db_service_catalog:git_path({host,"1.0.0"}),
   
 
    
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

cata_info_all()->
    
    A=[{boot,"1.0.0","https://github.com/joq62/boot.git"},
       {bully,"0.1.0","https://github.com/joq62/bully.git"},
       {conbee,"1.0.0","https://github.com/joq62/conbee.git"},
       {controller,"1.0.0",
	"https://github.com/joq62/controller.git"},
       {dbase_infra,"1.0.0",
	"https://github.com/joq62/dbase_infra.git"},
       {host,"1.0.0","https://github.com/joq62/host.git"},
       {kublet,"0.1.0","https://github.com/joq62/kublet.git"},
       {logger_infra,"1.0.0",
	"https://github.com/joq62/logger_infra.git"},
       {myadd,"1.0.0","https://github.com/joq62/myadd.git"},
       {mydivi,"1.0.0","https://github.com/joq62/mydivi.git"},
       {mymath,"1.0.0","https://github.com/joq62/mymath.git"},
       {sd,"1.0.0","https://github.com/joq62/sd.git"}],
    lists:keysort(1,A).
