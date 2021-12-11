%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(host_test).   
   
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

%    io:format("~p~n",[{"Start all_info()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=all_info(),
    io:format("~p~n",[{"Stop all_info()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start access()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=access(),
    io:format("~p~n",[{"Stop access()",?MODULE,?FUNCTION_NAME,?LINE}]),

 
%   io:format("~p~n",[{"Start start_args()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=start_args(),
    io:format("~p~n",[{"Stop start_args()",?MODULE,?FUNCTION_NAME,?LINE}]),

%   io:format("~p~n",[{"Start detailed()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=detailed(),
    io:format("~p~n",[{"Stop detailed()",?MODULE,?FUNCTION_NAME,?LINE}]),

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
all_info()->
    % init 
    AllInfo=access_info_all(),
    AllInfo=lists:keysort(1,db_host:read_all()),
    [{hostname,"c100"},
     {ip,"192.168.0.100"},
     {ssh_port,22},
     {uid,"joq62"},
     {pwd,"festum01"},
     {node,host0@c100}]=db_host:access_info({"c100","host0"}),
    
    auto_erl_controller=db_host:type({"c100","host0"}),
    [{erl_cmd,"/lib/erlang/bin/erl -detached"},
     {cookie,"cookie"},
     {env_vars,
      [{kublet,[{mode,controller}]},
       {dbase_infra,
	[{nodes,[host0@c100,host2@c100]}]},
       {bully,[{nodes,[host0@c100,host2@c100]}]}]},
     {nodename,"host1"}]=db_host:start_args({"c100","host1"}),
    ["logs"]=db_host:dirs_to_keep({"c100","host0"}),
    "applications"=db_host:application_dir({"c100","host2"}),
    stopped=db_host:status({"c100","host0"}),
   
    ok. 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
access()->
    [{hostname,"c100"},
     {ip,"192.168.0.100"},
     {ssh_port,22},
     {uid,"joq62"},
     {pwd,"festum01"},
     {node,host2@c100}]=db_host:access_info({"c100","host2"}),
   
    

    
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
%% -------------------------------------------------------------------


detailed()->
    
    "192.168.0.100"=db_host:ip({"c100","host2"}),
    22=db_host:port({"c100","host2"}),
    "joq62"=db_host:uid({"c100","host2"}),
    "festum01"=db_host:passwd({"c100","host2"}),
    host2@c100=db_host:node({"c100","host2"}),
    
    "/lib/erlang/bin/erl -detached"=db_host:erl_cmd({"c100","host2"}),
    [{kublet,[{mode,controller}]},
     {dbase_infra,[{nodes,[host0@c100,host1@c100]}]},
     {bully,[{nodes,[host0@c100,host1@c100]}]}]=db_host:env_vars({"c100","host2"}),
    "host2"=db_host:nodename({"c100","host2"}),
    "cookie"=db_host:cookie({"c100","host2"}),

    stopped=db_host:status({"c100","host2"}),
    {atomic,ok}=db_host:update_status({"c100","host2"},started),
    started=db_host:status({"c100","host2"}),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_args()->
  [{erl_cmd,"/lib/erlang/bin/erl -detached"},
   {cookie,"cookie"},
   {env_vars,
    [{kublet,[{mode,controller}]},
     {dbase_infra,
      [{nodes,[host0@c100,host1@c100]}]},
     {bully,[{nodes,[host0@c100,host1@c100]}]}]},
   {nodename,"host2"}]=db_host:start_args({"c100","host2"}),

    ok.
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
-define(ConfigDir,"test_configurations/host_configuration").
-define(Extension,".host").

setup()->
    %%--- Mnesia start
    application:start(dbase_infra),
    dbase_infra:load_from_file(db_host,?ConfigDir),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    application:stop(dbase_infra),
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
