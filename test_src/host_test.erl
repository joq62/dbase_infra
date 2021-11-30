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

 %   io:format("~p~n",[{"Start node_status()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=node_status(),
 %   io:format("~p~n",[{"Stop node_status()",?MODULE,?FUNCTION_NAME,?LINE}]),

%   io:format("~p~n",[{"Start start_args()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=start_args(),
 %   io:format("~p~n",[{"Stop start_args()",?MODULE,?FUNCTION_NAME,?LINE}]),

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
    AllInfo=db_host:read_all(),
    [{hostname,"c200"},
     {ip,"192.168.0.200"},
     {ssh_port,22},
     {uid,"joq62"},
     {pwd,"festum01"},
     {node,host@c200}]=db_host:access_info("c200"),
    
    auto_erl_controller=db_host:type("c200"),
    [{erl_cmd,"/lib/erlang/bin/erl -detached"},
     {cookie,"cookie"},
     {env_vars,
      [{kublet,[{mode,controller}]},
       {dbase_infra,[{nodes,[host@201,host@202]}]},
       {bully,[{nodes,[host@201,host@202]}]}]},
     {nodename,"host"}]=db_host:start_args("c200"),
    ["logs"]=db_host:dirs_to_keep("c200"),
    "applications"=db_host:application_dir("c200"),
    stopped=db_host:status("c200"),
   
    ok. 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
access()->
    [{hostname,"c200"},
     {ip,"192.168.0.200"},
     {ssh_port,22},
     {uid,"joq62"},
     {pwd,"festum01"},
     {node,host@c200}]=db_host:access_info("c200"),
   
    

    
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
    
    "192.168.0.201"=db_host:ip("c201"),
    22=db_host:port("c201"),
    "joq62"=db_host:uid("c201"),
    "festum01"=db_host:passwd("c201"),
    host@c201=db_host:node("c201"),

    "/lib/erlang/bin/erl -detached"=db_host:erl_cmd("c201"),
    [{kublet,[{mode,controller}]},
     {dbase_infra,[{nodes,[host@200,host@202]}]},
     {bully,[{nodes,[host@200,host@202]}]}]=db_host:env_vars("c201"),
    "host"=db_host:nodename("c201"),
    "cookie"=db_host:cookie("c201"),

    stopped=db_host:status("c200"),
    {atomic,ok}=db_host:update_status("c200",started),
    started=db_host:status("c200"),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_args()->
   [{erl_cmd,"/lib/erlang/bin/erl -detached"},
    {cookie,"cookie"},
    {env_vars,[{kublet,[{mode,controller}]}]},
    {nodename,"host"}]=host_config:start_args("c200"),
    
    [{erl_cmd,"/snap/erlang/current/usr/bin/erl -detached"},
     {cookie,"cookie"},
     {env_vars,[{kublet,[{mode,worker}]}]},
     {nodename,"host"}]=host_config:start_args("c203"),
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
os_status()->
    Started=lib_status:os_started(),
    Stopped=lib_status:os_stopped(),
    io:format("Started = ~p~n",[Started]),
    io:format("Stopped = ~p~n",[Stopped]),
    
    io:format("Started(c200) = ~p~n",[lib_status:os_started("c200")]),
    io:format("Started(c100) = ~p~n",[lib_status:os_started("c100")]),
    io:format("Stopped(c203) = ~p~n",[lib_status:os_stopped("c203")]),

    
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
node_status()->
    Started=lib_status:node_started(),
    Stopped=lib_status:node_stopped(),
    io:format("Started = ~p~n",[Started]),
    io:format("Stopped = ~p~n",[Stopped]),
    
    io:format("Started(c200) = ~p~n",[lib_status:node_started("c200")]),
    io:format("Started(c100) = ~p~n",[lib_status:node_started("c100")]),
    io:format("Stopped(c203) = ~p~n",[lib_status:node_stopped("c203")]),

    
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
-define(ConfigDir,"host_configuration").
-define(Extension,".host").

setup()->
    %%--- Mnesia start
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    mnesia:start(), 
    %%-- init_host
    db_host:create_table(),
    
     [db_host:create(HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status)||
	{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}<-host_info()],
    ok.

host_info()->
    {ok,Files}=file:list_dir(?ConfigDir),
    HostFiles=[File||File<-Files,
		     ?Extension=:=filename:extension(File)],
    HostFileNames=[filename:join(?ConfigDir,File)||File<-HostFiles],
    AccessInfo=create_list(HostFileNames),
    AccessInfo.

create_list(HostFileNames)->
    create_list(HostFileNames,[]).
create_list([],List)->
   % io:format("List ~p~n",[List]),
    List;
create_list([HostFile|T],Acc)->
    {ok,I}=file:consult(HostFile),
    HostName=proplists:get_value(hostname,I),
    StartArgs=proplists:get_value(start_args,I),
    AccessInfo=proplists:get_value(access_info,I),
    Type=proplists:get_value(host_type,I),
    DirsToKeep=proplists:get_value(dirs_to_keep,I),
    AppDir=proplists:get_value(application_dir,I),
    Status=stopped,
   % io:format("~p~n",[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}]),
    NewAcc=[{HostName,AccessInfo,Type,StartArgs,DirsToKeep,AppDir,Status}|Acc],
    
    create_list(T,NewAcc).
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
[{"c202",
             [{hostname,"c202"},
              {ip,"192.168.0.202"},
              {ssh_port,22},
              {uid,"joq62"},
              {pwd,"festum01"},
              {node,host@c202}],
             auto_erl_controller,
             [{erl_cmd,"/lib/erlang/bin/erl -detached"},
              {cookie,"cookie"},
              {env_vars,
               [{kublet,[{mode,controller}]},
                {dbase_infra,[{nodes,[host@200,host@201]}]},
                {bully,[{nodes,[host@200,host@201]}]}]},
              {nodename,"host"}],
             ["logs"],
             "applications",stopped},
            {"c203",
             [{hostname,"c203"},
              {ip,"192.168.0.203"},
              {ssh_port,22},
              {uid,"pi"},
              {pwd,"festum01"},
              {node,host@c203}],
             non_auto_erl_worker,
             [{erl_cmd,"/snap/erlang/current/usr/bin/erl -detached"},
              {cookie,"cookie"},
              {env_vars,[{kublet,[{mode,worker}]}]},
              {nodename,"host"}],
             ["logs"],
             "applications",stopped},
            {"c201",
             [{hostname,"c201"},
              {ip,"192.168.0.201"},
              {ssh_port,22},
              {uid,"joq62"},
              {pwd,"festum01"},
              {node,host@c201}],
             undefined,
             [{erl_cmd,"/lib/erlang/bin/erl -detached"},
              {cookie,"cookie"},
              {env_vars,
               [{kublet,[{mode,controller}]},
                {dbase_infra,[{nodes,[host@200,host@202]}]},
                {bully,[{nodes,[host@200,host@202]}]}]},
              {nodename,"host"}],
             ["logs"],
             "applications",stopped},
            {"c200",
             [{hostname,"c200"},
              {ip,"192.168.0.200"},
              {ssh_port,22},
              {uid,"joq62"},
              {pwd,"festum01"}, 
              {node,host@c200}],
             auto_erl_controller,
             [{erl_cmd,"/lib/erlang/bin/erl -detached"},
              {cookie,"cookie"},
              {env_vars,
               [{kublet,[{mode,controller}]},
                {dbase_infra,[{nodes,[host@201,host@202]}]},
                {bully,[{nodes,[host@201,host@202]}]}]},
              {nodename,"host"}],
             ["logs"],
             "applications",stopped}].
