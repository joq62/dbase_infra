%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(distributed_test).   
   
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
  %  io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start cluster_start()()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=cluster_start(),
%    io:format("~p~n",[{"Stop cluster_start()",?MODULE,?FUNCTION_NAME,?LINE}]),


    io:format("~p~n",[{"Start initial()()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=initial(),
    io:format("~p~n",[{"Stop initial()()",?MODULE,?FUNCTION_NAME,?LINE}]),


 %   
      %% End application tests
  %  io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
  %  io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
initial()->
    [ok,ok,ok]=[rpc:call(N,application,start,[sd],5*1000)||N<-get_nodes()],
    [N1,N2,N3]=get_nodes(),
    %% intial node
    ok=rpc:call(N1,application,start,[dbase_infra],5*1000),
    [io:format("N1 ~p~n",[{Node,rpc:call(Node,mnesia,system_info,[tables],2*1000)}])||Node<-get_nodes()],

    ok=rpc:call(N2,application,start,[dbase_infra],5*1000),
    [io:format("N2 ~p~n",[{Node,rpc:call(Node,mnesia,system_info,[tables],2*1000)}])||Node<-get_nodes()],

    ok=rpc:call(N3,application,start,[dbase_infra],5*1000),
    [io:format("N3 ~p~n",[{Node,rpc:call(Node,mnesia,system_info,[tables],2*1000)}])||Node<-get_nodes()],

    [io:format("service_catalog ~p~n",[{Node,rpc:call(Node,db_service_catalog,read_all,[],2*1000)}])||Node<-get_nodes()],
    
  %  [ok,ok,ok]=[rpc:call(Node,application,start,[dbase_infra],5*1000)||Node<-get_nodes()],
 %   [io:format("~p~n",[{Node,rpc:call(Node,mnesia,system_info,[],2*1000)}])||Node<-get_nodes()],
    %%----- load initial node
%    [Node0|_]=get_nodes(),
 %   [{atomic,ok},{atomic,ok},{atomic,ok}]=rpc:call(Node0,dbase_infra,load_from_file,[db_host,?ConfigDir],5*1000),
    
 %   [{host0@c100,host1@c100},
  %   {host1@c100,{badrpc,_}},
  %   {host2@c100,{badrpc,_}}]=[{Node,rpc:call(Node,db_host,node,[{"c100","host1"}],5*1000)}||Node<-get_nodes()],
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_nodes()->
    HostId=net_adm:localhost(),
    A="host0@"++HostId,
    Node0=list_to_atom(A),
    B="host1@"++HostId,
    Node1=list_to_atom(B),
    C="host2@"++HostId,
    Node2=list_to_atom(C),    
    [Node0,Node1,Node2].
    
start_slave(NodeName)->
    HostId=net_adm:localhost(),
    Node=list_to_atom(NodeName++"@"++HostId),
    rpc:call(Node,init,stop,[]),
    Cookie=atom_to_list(erlang:get_cookie()),
    Args="-pa ebin -setcookie "++Cookie,
    slave:start(HostId,NodeName,Args).

setup()->
    HostId=net_adm:localhost(),
    A="host0@"++HostId,
    Node0=list_to_atom(A),
    B="host1@"++HostId,
    Node1=list_to_atom(B),
    C="host2@"++HostId,
    Node2=list_to_atom(C),    
    Nodes=[Node0,Node1,Node2],
    [rpc:call(N,init,stop,[],5*1000)||N<-Nodes],
    timer:sleep(2000),
    [{ok,Node0},
     {ok,Node1},
     {ok,Node2}]=[start_slave(NodeName)||NodeName<-["host0","host1","host2"]],
    [net_adm:ping(N)||N<-Nodes],
    application:stop(dbase_infra),
    application:unload(dbase_infra),
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    timer:sleep(1000),
      
  
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
