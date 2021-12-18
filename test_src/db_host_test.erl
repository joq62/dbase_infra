%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_host_test).   
    
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
    AllInfo=host_info_all(),
    AllInfo=lists:keysort(1,db_host:read_all()),
   [{hostname,"c100"},
    {ip,"192.168.0.100"},
    {ssh_port,22},
    {uid,"joq62"},
    {pwd,"festum01"},
    {node,host1@c100}]=db_host:access_info({"c100","host1"}),
    
    auto_erl_controller=db_host:type({"c100","host2"}),
    [{erl_cmd,"erl -detached"},
     {cookie,"cookie_test"},
     {env_vars,
      [{kublet,[{mode,controller}]},
       {dbase_infra,
	[{nodes,[host1@c100,host2@c100,host4@c100]}]},
       {bully,[{nodes,[host1@c100,host2@c100,host4@c100]}]}]},
     {nodename,"host3"}]=db_host:start_args({"c100","host3"}),

    ["logs"]=db_host:dirs_to_keep({"c100","host2"}),
    "host1.applications"=db_host:application_dir({"c100","host1"}),
    stopped=db_host:status({"c100","host2"}),

   "192.168.0.100"=db_host:ip({"c100","host1"}),
    22=db_host:port({"c100","host2"}),
    "joq62"=db_host:uid({"c100","host2"}),
    "festum01"=db_host:passwd({"c100","host1"}),
    host1@c100=db_host:node({"c100","host1"}),
    
    "erl -detached"=db_host:erl_cmd({"c100","host1"}),
    [{kublet,[{mode,controller}]},
     {dbase_infra,[{nodes,[host1@c100,host2@c100,host4@c100]}]},
     {bully,[{nodes,[host1@c100,host2@c100,host4@c100]}]}]=db_host:env_vars({"c100","host3"}),
    "host1"=db_host:nodename({"c100","host1"}),
    "cookie_test"=db_host:cookie({"c100","host1"}),

    stopped=db_host:status({"c100","host1"}),
    {atomic,ok}=db_host:update_status({"c100","host1"},started),
    started=db_host:status({"c100","host1"}),
   
    [{hostname,"c100"},
     {ip,"192.168.0.100"},
     {ssh_port,22},
     {uid,"joq62"},
     {pwd,"festum01"},
     {node,host1@c100}]=db_host:access_info({"c100","host1"}),
   
    [{erl_cmd,"erl -detached"},
     {cookie,"cookie_test"},
     {env_vars,
      [{kublet,[{mode,controller}]},
       {dbase_infra,
	[{nodes,[host1@c100,host2@c100,host4@c100]}]},
       {bully,[{nodes,[host1@c100,host2@c100,host4@c100]}]}]},
     {nodename,"host3"}]=db_host:start_args({"c100","host3"}),  
    
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

host_info_all()->
    
    A= [{{"c100","host1"},
	 [{hostname,"c100"},
	  {ip,"192.168.0.100"},
	  {ssh_port,22},
	  {uid,"joq62"},
	  {pwd,"festum01"},
	  {node,host1@c100}],
	 auto_erl_controller,
	 [{erl_cmd,"erl -detached"},
	  {cookie,"cookie_test"},
	  {env_vars,
	   [{kublet,[{mode,controller}]},
	    {dbase_infra,
	     [{nodes,[host2@c100,host3@c100,host4@c100]}]},
	    {bully,[{nodes,[host2@c100,host3@c100,host4@c100]}]}]},
	  {nodename,"host1"}],
	 ["logs"],
	 "host1.applications",stopped},
	{{"c100","host2"},
	 [{hostname,"c100"},
	  {ip,"192.168.0.100"},
	  {ssh_port,22},
	  {uid,"joq62"},
	  {pwd,"festum01"},
	  {node,host2@c100}], 
	 auto_erl_controller,
	 [{erl_cmd,"erl -detached"},
	  {cookie,"cookie_test"},
	  {env_vars,
	   [{kublet,[{mode,controller}]},
	    {dbase_infra,
	     [{nodes,[host1@c100,host3@c100,host4@c100]}]},
	    {bully,[{nodes,[host1@c100,host3@c100,host4@c100]}]}]},
	  {nodename,"host2"}],
	 ["logs"],
	 "host2.applications",stopped},
	{{"c100","host3"},
	 [{hostname,"c100"},
	  {ip,"192.168.0.100"},
	  {ssh_port,22},
	  {uid,"joq62"},
	  {pwd,"festum01"},
	  {node,host3@c100}],
	 auto_erl_controller,
	 [{erl_cmd,"erl -detached"},
	  {cookie,"cookie_test"},
	  {env_vars,
	   [{kublet,[{mode,controller}]},
	    {dbase_infra,
	     [{nodes,[host1@c100,host2@c100,host4@c100]}]},
	    {bully,[{nodes,[host1@c100,host2@c100,host4@c100]}]}]},
	  {nodename,"host3"}],
	 ["logs"],
	 "host3.applications",stopped},
	{{"c100","host4"},
	 [{hostname,"c100"},
	  {ip,"192.168.0.100"},
	  {ssh_port,22},
	  {uid,"joq62"},
	  {pwd,"festum01"},
	  {node,host4@c100}],
	 no_auto_erl_worker,
	 [{erl_cmd,"erl -detached"},
	  {cookie,"cookie_test"},
	  {env_vars,[]},
	  {nodename,"host4"}],
	 ["logs"],
	 "host4.applications",stopped}],
    lists:keysort(1,A).
