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
    ok=cluster_start(),
    io:format("~p~n",[{"Stop cluster_start()",?MODULE,?FUNCTION_NAME,?LINE}]),


%    io:format("~p~n",[{"Start initial()()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=initial(),
 %   io:format("~p~n",[{"Stop initial()()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start add_node()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=add_node(),
%    io:format("~p~n",[{"Stop add_node()",?MODULE,?FUNCTION_NAME,?LINE}]),

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
cluster_start()->

 %   io:format("get_nodes()~p~n",[{lib_bully:get_nodes(),
%				 ?MODULE,?FUNCTION_NAME,?LINE}]),
    Nodes=nodes(),
    [N1,N2,N3]=Nodes,
    io:format("N1,N2,N3 ~p~n",[{N1,N2,N3,?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Start sd and bully on all nodes
    [ok,ok,ok]=[rpc:call(N,application,start,[sd],5*1000)||N<-Nodes],
    [ok,ok,ok]=[rpc:call(N,application,start,[bully],5*1000)||N<-Nodes],
    timer:sleep(1000),    
    N1=rpc:call(N1,bully,who_is_leader,[],5*1000),

    % Start first node

    ok=rpc:call(N1,application,start,[dbase_infra],5*1000),
    ok=rpc:call(N1,dbase_infra,init_dynamic,[],5*1000),
    timer:sleep(1000),
    
    io:format("N1,mnesia:system_info()~p~n",[{rpc:call(N1,mnesia,system_info,[],5*1000),
					   ?MODULE,?FUNCTION_NAME,?LINE}]),
 io:format("N1,mnesia:system_info(tables)~p~n",[{rpc:call(N1,mnesia,system_info,[tables],5*1000),
					   ?MODULE,?FUNCTION_NAME,?LINE}]),


    % Start second node
    ok=rpc:call(N2,application,start,[dbase_infra],5*1000),
    
    ok=rpc:call(N1,dbase_infra,add_dynamic,[N2],5*1000),
    timer:sleep(1000),
    io:format("N1,mnesia:system_info()~p~n",[{rpc:call(N1,mnesia,system_info,[],5*1000),
					   ?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("N2,mnesia:system_info()~p~n",[{rpc:call(N2,mnesia,system_info,[],5*1000),
					   ?MODULE,?FUNCTION_NAME,?LINE}]),
   
    
  % Start third node

    ok=rpc:call(N3,application,start,[dbase_infra],5*1000),
    ok=rpc:call(N1,dbase_infra,add_dynamic,[N3],5*1000),
    io:format("N1,N2,N3 get_nodes()~p~n",[{lib_bully:get_nodes(),
					?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("N1 leader ~p~n",[{rpc:call(N1,bully,who_is_leader,[],5*1000),
				 ?MODULE,?FUNCTION_NAME,?LINE}]),
    N1=rpc:call(N1,bully,who_is_leader,[],5*1000),
    N1=rpc:call(N2,bully,who_is_leader,[],5*1000),
    N1=rpc:call(N3,bully,who_is_leader,[],5*1000),

    io:format("N2,mnesia:system_info()~p~n",[{rpc:call(N2,mnesia,system_info,[],5*1000),
					   ?MODULE,?FUNCTION_NAME,?LINE}]),
    
    
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
