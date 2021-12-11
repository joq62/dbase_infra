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
    AllInfo=host_info_all(),
    AllInfo=lists:keysort(1,db_host:read_all()),
   [{hostname,"c200"},
    {ip,"192.168.0.200"},
    {ssh_port,22},
    {uid,"joq62"},
    {pwd,"festum01"},
    {node,host@c200}]=db_host:access_info({"c200","host"}),
    
    auto_erl_controller=db_host:type({"c200","host"}),
    [{erl_cmd,"erl -detached"},
     {cookie,"cookie"},
     {env_vars,
      [{kublet,[{mode,controller}]},
       {dbase_infra,[{nodes,[host@c201,host@c202]}]},
       {bully,[{nodes,[host@c201,host@c202]}]}]},
     {nodename,"host"}]=db_host:start_args({"c200","host"}),
    ["logs"]=db_host:dirs_to_keep({"c200","host"}),
    "host.applications"=db_host:application_dir({"c200","host"}),
    stopped=db_host:status({"c200","host"}),

   "192.168.0.200"=db_host:ip({"c200","host"}),
    22=db_host:port({"c200","host"}),
    "joq62"=db_host:uid({"c200","host"}),
    "festum01"=db_host:passwd({"c200","host"}),
    host@c200=db_host:node({"c200","host"}),
    
    "erl -detached"=db_host:erl_cmd({"c200","host"}),
    [{kublet,[{mode,controller}]},
     {dbase_infra,[{nodes,[host@c201,host@c202]}]},
     {bully,[{nodes,[host@c201,host@c202]}]}]=db_host:env_vars({"c200","host"}),
    "host"=db_host:nodename({"c200","host"}),
    "cookie"=db_host:cookie({"c200","host"}),

    stopped=db_host:status({"c200","host"}),
    {atomic,ok}=db_host:update_status({"c200","host"},started),
    started=db_host:status({"c200","host"}),
   
    [{hostname,"c200"},
     {ip,"192.168.0.200"},
     {ssh_port,22},
     {uid,"joq62"},
     {pwd,"festum01"},
     {node,host@c200}]=db_host:access_info({"c200","host"}),
   
    [{erl_cmd,"erl -detached"},
     {cookie,"cookie"},
     {env_vars,
      [{kublet,[{mode,controller}]},
       {dbase_infra,[{nodes,[host@c201,host@c202]}]},
       {bully,[{nodes,[host@c201,host@c202]}]}]},
     {nodename,"host"}]=db_host:start_args({"c200","host"}),  
    
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
    
    A=[{{"c200","host"},
	[{hostname,"c200"},
	 {ip,"192.168.0.200"},
	 {ssh_port,22},
	 {uid,"joq62"}, 
	 {pwd,"festum01"},
	 {node,host@c200}],
	auto_erl_controller,
	[{erl_cmd,"erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,
	  [{kublet,[{mode,controller}]},
	   {dbase_infra,[{nodes,[host@c201,host@c202]}]},
	   {bully,[{nodes,[host@c201,host@c202]}]}]},
	 {nodename,"host"}],
	["logs"],
	"host.applications",stopped},
       {{"c201","host"},
	[{hostname,"c201"},
	 {ip,"192.168.0.201"},
	 {ssh_port,22},
	 {uid,"joq62"},
	 {pwd,"festum01"},
	 {node,host@c201}],
	auto_erl_controller,
	[{erl_cmd,"erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,
	  [{dbase_infra,[{nodes,[host@c200,host@c202]}]},
	   {bully,[{nodes,[host@c200,host@c202]}]}]},
	 {nodename,"host"}],
	["logs"],
	"host.applications",stopped},
       {{"c202","host"},
	[{hostname,"c202"},
	 {ip,"192.168.0.202"},
	 {ssh_port,22},
	 {uid,"joq62"},
	 {pwd,"festum01"},
	 {node,host@c202}],
	auto_erl_controller,
	[{erl_cmd,"erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,
	  [{dbase_infra,[{nodes,[host@c200,host@c201]}]},
	   {bully,[{nodes,[host@c200,host@c201]}]}]},
	 {nodename,"host"}],
	["logs"],
	"host.applications",stopped},
       {{"c203","host"},
	[{hostname,"c203"},
	 {ip,"192.168.0.203"},
	 {ssh_port,22},
	 {uid,"pi"},
	 {pwd,"festum01"},
	 {node,host@c203}],
	non_auto_erl_worker,
	[{erl_cmd,"/snap/erlang/current/usr/bin/erl -detached"},
	 {cookie,"cookie"},
	 {env_vars,[{glurk,[{mode,glurk}]}]},
	 {nodename,"host"}],
	["logs"],
	"host.applications",stopped}],
    lists:keysort(1,A).
