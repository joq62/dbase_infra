%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
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
  %  io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start db_host_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=db_host_test:start(),
    io:format("~p~n",[{"Stop db_host_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %  io:format("~p~n",[{"Start db_deployment_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=db_deployment_test:start(),
    io:format("~p~n",[{"Stop db_deployment_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

%  io:format("~p~n",[{"Start db_pods_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=db_pods_test:start(),
    io:format("~p~n",[{"Stop db_pods_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

%  io:format("~p~n",[{"Start db_service_catalog_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=db_service_catalog_test:start(),
    io:format("~p~n",[{"Stop db_service_catalog_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

%  io:format("~p~n",[{"Start db_deploy_state_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=db_deploy_state_test:start(),
    io:format("~p~n",[{"Stop db_deploy_state_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

%  io:format("~p~n",[{"Start db_logger_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=db_logger_test:start(),
    io:format("~p~n",[{"Stop db_logger_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %  io:format("~p~n",[{"Start distributed_test()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=distributed_test:start(),
    io:format("~p~n",[{"Stop distributed_test()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start monkey()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=monkey(),
  %  io:format("~p~n",[{"Stop monkey()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   
      %% End application tests
 %   io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
 %   io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->
    ok=application:start(sd),
    ok=application:start(dbase_infra),
    io:format("app ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
