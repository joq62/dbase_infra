%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_pods_test).   
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
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
    AllInfo=pods_info_all(),
    AllInfo=lists:keysort(1,db_pods:read_all()),
    ["myadd","mydivi_c200","infra","sd",
     "mydivi_c201","mydivi","mydivi_c202",
     "mydivi_c203","single_mymath"]=db_pods:name(),
    
    "myadd"=db_pods:name({"myadd","1.0.0"}),
    "1.0.0"=db_pods:vsn({"mydivi_c202","1.0.0"}),
    [{"myadd","1.0.0"},
     {"mydivi_c200","1.0.0"},
     {"infra","1.0.0"},
     {"sd","1.0.0"},
     {"mydivi_c201","1.0.0"},
     {"mydivi","1.0.0"},
     {"mydivi_c202","1.0.0"},
     {"mydivi_c203","1.0.0"},
     {"single_mymath","1.0.0"}]=db_pods:ids(),
   
    [{mydivi,"1.0.0"},{sd,"1.0.0"}]=db_pods:application({"mydivi_c202","1.0.0"}),

    {preffered,[{"c100","host3"}]}=db_pods:host({"mydivi_c202","1.0.0"}),
    {no_preference,[]}=db_pods:host({"sd","1.0.0"}),
    
    
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

pods_info_all()->
    
    A=[{{"infra","1.0.0"},
	"infra","1.0.0",
	[{controller,"0.1.0"},
	 {dbase_infra,"1.0.0"},
	 {sd,"1.0.0"},
	 {host,"1.0.0"}],
	{no_preference,[]}},
       {{"myadd","1.0.0"},
	"myadd","1.0.0",
	[{myadd,"1.0.0"},{sd,"1.0.0"}],
	{preffered,[{"c100","host1"}]}},
       {{"mydivi","1.0.0"},
	"mydivi","1.0.0",
	[{mydivi,"1.0.0"},{sd,"1.0.0"}],
	{preffered,[]}},
       {{"mydivi_c200","1.0.0"},
	"mydivi_c200","1.0.0",
	[{mydivi,"1.0.0"},{sd,"1.0.0"}],
	{preffered,[{"c100","host1"}]}},
       {{"mydivi_c201","1.0.0"},
	"mydivi_c201","1.0.0",
	[{mydivi,"1.0.0"},{sd,"1.0.0"}],
	{preffered,[{"c100","host2"}]}},
       {{"mydivi_c202","1.0.0"},
	"mydivi_c202","1.0.0",
	[{mydivi,"1.0.0"},{sd,"1.0.0"}],
	{preffered,[{"c100","host3"}]}},
       {{"mydivi_c203","1.0.0"},
	"mydivi_c203","1.0.0",
	[{mydivi,"1.0.0"},{sd,"1.0.0"}],
	{preffered,[{"c100","host4"}]}},
       {{"sd","1.0.0"},
	"sd","1.0.0",
	[{sd,"1.0.0"}],
	{no_preference,[]}},
       {{"single_mymath","1.0.0"},
	"single_mymath","1.0.0",
	[{mymath,"1.0.0"},
	 {myadd,"1.0.0"},
	 {mydivi,"1.0.0"},
	 {sd,"1.0.0"}],
	{preferred,[{"c100","host3"}]}}],
    lists:keysort(1,A).
