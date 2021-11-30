%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(host_config).   
 
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ConfigDir,"host_configuration").
-define(Extension,".host").

%% --------------------------------------------------------------------


%% External exports
-export([
	 access_info/0,
	 access_info/1,
	 host/0,
	 start_args/1,
	 ip/1,
	 ssh_port/1,
	 uid/1,
	 passwd/1,
	 node/1,
	 erl_cmd/1,
	 cookie/1,
	 env_vars/1,
	 nodename/1
	
	 
	]).



%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
erl_cmd(HostName)->
    StartArgs=start_args(HostName),
    proplists:get_value(erl_cmd,StartArgs).

env_vars(HostName)->
    StartArgs=start_args(HostName),
    proplists:get_value(env_vars,StartArgs).

nodename(HostName)->
    StartArgs=start_args(HostName),
    proplists:get_value(nodename,StartArgs).

cookie(HostName)->
    StartArgs=start_args(HostName),
    proplists:get_value(cookie,StartArgs).

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

ip(HostName)->
    AccesInfo=access_info(HostName),
    proplists:get_value(ip,AccesInfo).

ssh_port(HostName)->
    AccesInfo=access_info(HostName),
    proplists:get_value(ssh_port,AccesInfo).

uid(HostName)->
    AccesInfo=access_info(HostName),
    proplists:get_value(uid,AccesInfo).

passwd(HostName)->
    AccesInfo=access_info(HostName),
    proplists:get_value(pwd,AccesInfo).

node(HostName)->
    AccesInfo=access_info(HostName),
    proplists:get_value(node,AccesInfo).


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start_args(HostName)->
  AllHostInfo=access_info(),
    R=[proplists:get_value(start_args,Info)||Info<-AllHostInfo,
					     HostName=:=proplists:get_value(hostname,Info)],
    case R of
	[I]->
	    I;
	undefined->
	    undefined
    end.
	
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
host()->
    AllHostInfo=access_info(),
    AccessInfoList=[proplists:get_value(access_info,AccessInfo)||AccessInfo<-AllHostInfo],
    Hosts=[proplists:get_value(hostname,AccessInfo)||AccessInfo<-AccessInfoList],
    Hosts.
    


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
access_info()->
    {ok,Files}=file:list_dir(?ConfigDir),
    HostFiles=[File||File<-Files,
		     ?Extension=:=filename:extension(File)],
    HostFileNames=[filename:join(?ConfigDir,File)||File<-HostFiles],
    AccessInfo=create_list(HostFileNames),
    AccessInfo.
access_info(HostName)->
    AllHostInfo=access_info(),
    R=[proplists:get_value(access_info,AccessInfo)||AccessInfo<-AllHostInfo,
								 HostName=:=proplists:get_value(hostname,AccessInfo)],
   % R=[Info||Info<-AccessInfoList,
%	   HostName=:=proplists:get_value(hostname,Info)],
    case R of
	[I]->
	    I;
	undefined->
	    undefined
    end.
	
   
create_list(HostFileNames)->
    create_list(HostFileNames,[]).
create_list([],List)->
    List;
create_list([HostFile|T],Acc)->
    {ok,I}=file:consult(HostFile),
    create_list(T,[I|Acc]).


    


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
