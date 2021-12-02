%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").
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

%    io:format("~p~n",[{"Start initial()()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=initial(),
    io:format("~p~n",[{"Stop initial()()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start add_node()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=add_node(),
    io:format("~p~n",[{"Stop add_node()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start node_status()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=node_status(),
 %   io:format("~p~n",[{"Stop node_status()",?MODULE,?FUNCTION_NAME,?LINE}]),

%   io:format("~p~n",[{"Start start_args()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=start_args(),
 %   io:format("~p~n",[{"Stop start_args()",?MODULE,?FUNCTION_NAME,?LINE}]),

%   io:format("~p~n",[{"Start detailed()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=detailed(),
%    io:format("~p~n",[{"Stop detailed()",?MODULE,?FUNCTION_NAME,?LINE}]),

%   io:format("~p~n",[{"Start start_stop()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=start_stop(),
 %   io:format("~p~n",[{"Stop start_stop()",?MODULE,?FUNCTION_NAME,?LINE}]),



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
-define(ConfigDir,"test_configurations/host_configuration").

initial()->
    [ok,ok,ok]=[rpc:call(Node,application,start,[dbase_infra],5*1000)||Node<-get_nodes()],
 %   [io:format("~p~n",[{Node,rpc:call(Node,mnesia,system_info,[],2*1000)}])||Node<-get_nodes()],
    %%----- load initial node
    [Node0|_]=get_nodes(),
    [{atomic,ok},{atomic,ok},{atomic,ok}]=rpc:call(Node0,dbase_infra,load_from_file,[db_host,?ConfigDir],5*1000),
    
    [{host0@c100,host1@c100},
     {host1@c100,{badrpc,_}},
     {host2@c100,{badrpc,_}}]=[{Node,rpc:call(Node,db_host,node,[{"c100","host1"}],5*1000)}||Node<-get_nodes()],
    
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add_node()->
    [Node0,Node1,Node2]=get_nodes(),
    {badrpc,_}=rpc:call(Node1,db_host,node,[{"c100","host1"}]),
    {badrpc,_}=rpc:call(Node2,db_host,node,[{"c100","host1"}]),
    
    ok=rpc:call(Node1,dbase_infra,add_dynamic,[Node0],3*1000),
    timer:sleep(500),
    ok=rpc:call(Node1,dbase,dynamic_load_table,[db_host],3*1000),
    timer:sleep(500),
    host1@c100=rpc:call(Node1,db_host,node,[{"c100","host1"}]),
    {badrpc,_}=rpc:call(Node2,db_host,node,[{"c100","host1"}]),

    ok=rpc:call(Node2,dbase_infra,add_dynamic,[Node0],3*1000),
    timer:sleep(500),
    ok=rpc:call(Node2,dbase,dynamic_load_table,[db_host],3*1000),
    timer:sleep(500),
    host1@c100=rpc:call(Node1,db_host,node,[{"c100","host1"}]),
    host2@c100=rpc:call(Node2,db_host,node,[{"c100","host2"}]),
    
    %---------- stop and restart node
    slave:stop(Node0),
    {badrpc,_}=rpc:call(Node0,db_host,node,[{"c100","host1"}]),
    host1@c100=rpc:call(Node1,db_host,node,[{"c100","host1"}]),
    host2@c100=rpc:call(Node2,db_host,node,[{"c100","host2"}]),
    %% restart node
  
    {ok,Node0}=start_slave("host0"),
    {badrpc,_}=rpc:call(Node0,db_host,node,[{"c100","host1"}]),
    host1@c100=rpc:call(Node1,db_host,node,[{"c100","host1"}]),
    host2@c100=rpc:call(Node2,db_host,node,[{"c100","host2"}]),

    %% Start dbase_infra
    ok=rpc:call(Node0,application,start,[dbase_infra],5*1000),
   ok=rpc:call(Node0,dbase_infra,add_dynamic,[Node1],3*1000),
    timer:sleep(500),
    ok=rpc:call(Node0,dbase,dynamic_load_table,[db_host],3*1000),
    timer:sleep(500),

    host1@c100=rpc:call(Node0,db_host,node,[{"c100","host1"}]),
    host1@c100=rpc:call(Node1,db_host,node,[{"c100","host1"}]),
    host2@c100=rpc:call(Node2,db_host,node,[{"c100","host2"}]),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
x()->    
    [[{atomic,ok},{atomic,ok},{atomic,ok}],
     [{atomic,ok},{atomic,ok},{atomic,ok}],
     [{atomic,ok},{atomic,ok},{atomic,ok}]].





    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


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
    [{ok,Node0},
     {ok,Node1},
     {ok,Node2}]=[start_slave(NodeName)||NodeName<-["host0","host1","host2"]],
  
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

access_info_all()->
    
    A=[{{"c100","host0"},
	[{hostname,"c100"},
	 {ip,"192.168.0.100"},
	 {ssh_port,22},
	 {uid,"joq62"},
	 {pwd,"festum01"},
	 {node,host0@c100}],
	auto_erl_controller,
	[{erl_cmd,"/lib/erlang/bin/erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,
	  [{kublet,[{mode,controller}]},
	   {dbase_infra,[{nodes,[host1@c100,host2@c100]}]},
	   {bully,[{nodes,[host1@c100,host2@c100]}]}]},
	 {nodename,"host0"}],
	["logs"],
	"applications",stopped},
       {{"c100","host1"},
	[{hostname,"c100"},
	 {ip,"192.168.0.100"},
	 {ssh_port,22},
	 {uid,"joq62"},
	 {pwd,"festum01"},
	 {node,host1@c100}],
	auto_erl_controller,
	[{erl_cmd,"/lib/erlang/bin/erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,
	  [{kublet,[{mode,controller}]},
	   {dbase_infra,[{nodes,[host0@c100,host2@c100]}]},
	   {bully,[{nodes,[host0@c100,host2@c100]}]}]},
	 {nodename,"host1"}],
	["logs"],
	"applications",stopped},
       {{"c100","host2"},
	[{hostname,"c100"},
	 {ip,"192.168.0.100"},
	 {ssh_port,22},
	 {uid,"joq62"},
	 {pwd,"festum01"},
	 {node,host2@c100}],
	auto_erl_controller,
	[{erl_cmd,"/lib/erlang/bin/erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,
	  [{kublet,[{mode,controller}]},
	   {dbase_infra,[{nodes,[host0@c100,host1@c100]}]},
	   {bully,[{nodes,[host0@c100,host1@c100]}]}]},
	 {nodename,"host2"}],
	["logs"],
	"applications",stopped}],
    lists:keysort(1,A).
