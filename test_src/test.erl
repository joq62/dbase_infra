%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
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
    io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start init_start()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=init_start(),
  %  io:format("~p~n",[{"Stop init_start()",?MODULE,?FUNCTION_NAME,?LINE}]),
    
    io:format("~p~n",[{"Start pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pass_0(),
    io:format("~p~n",[{"Stop pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=pass_1(),
 %   io:format("~p~n",[{"Stop pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),
 
     %% End application tests
    io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
    io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_0()->
    io:format("sd:all 1 ~p~n",[sd:all()]),
  %  LoadedServices=[Service||{ok,Service}<-lib_controller:load_services()],    
    ok=application:start(controller),
    LoadedServices=controller:loaded(),
    io:format("LoadedServices=~p~n",[LoadedServices]),
    io:format("sd:all 2 ~p~n",[sd:all()]),
   % R=[{application:start(Application),Application}||Application<-LoadedServices],
    
 %   io:format("~p~n",[R]),
    spawn(fun()->server_sim() end),
    io:format("which applications =~p~n",[application:which_applications()]),    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
server_sim()->
    {ok,Node,AppPid}=loader:allocate(myadd),
    io:format("Node,AppPid = ~p~n",[{Node,AppPid}]),
    true=erlang:monitor_node(Node,true),
    erlang:monitor(process,AppPid),
    loop(Node,AppPid,5).

loop(_,_,0)->
    io:format("loop stopped ~n");
loop(Node,AppPid,N)->
    AppPid!{self(),add,[20,22+N]},
    receive
	{AppPid,{ok,R}}->
	    io:format("R = ~p~n",[R]),
	    timer:sleep(1000);
	X->
	    io:format("X = ~p~n",[X])
    after 1000 ->
	    io:format("timeout ~n")
    end,
    if
	N=:=3->
	    AppPid!{stop},
	    %true=erlang:exit(AppPid,stopped),
	    timer:sleep(100);
	N=:=2->
	    rpc:call(Node,init,stop,[]);
	true ->
	    ok
    end,
    loop(Node,AppPid,N-1).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_1()->
    % test that myadd is not starte on tes
    {badrpc,{'EXIT',{noproc,{gen_server,call,[myadd,{add,1,2},infinity]}}}}=
	rpc:call(node(),myadd,add,[1,2]),
      
    spawn(fun()->
		  test_proc() end),
  
    ok.
 
test_proc()->
    {ok,Node,AppRef}=allocate(myadd),
    {error,_}=rpc:call(Node,application,start,[mydivi],1000),
    42=rpc:call(Node,myadd,add,[20,22],1000),
    io:format("sd:all 1 ~p~n",[sd:all()]),
    % 
    
    %shutdown_ok=rpc:call(Node,myadd,stop,[],1000),
    deallocate(Node,myadd),
    
    receive
	{'DOWN',AppRef,process,Pid,normal}->
	    io:format("X ~p~n",[{'DOWN',AppRef,process,Pid,normal}]);
	{nodedown,Node}->
	    io:format("X ~p~n",[{nodedown,Node}])
    after 2000->
	    io:format("timeout X ~p~n",[1])
    end,
    %
    
    receive
	{'DOWN',AppRef,process,Pid2,normal}->
	    io:format("Y ~p~n",[{'DOWN',AppRef,process,Pid2,normal}]);
	{nodedown,Node}->
	    io:format("Y ~p~n",[{nodedown,Node}])
    after 2000->
	    io:format("timeout Y ~p~n",[1])
    end,
  %  ok=rpc:call(Node,init,stop,[],1000),
  %  timer:sleep(10),
  %  receive
%	{nodedown,Node}->
%	    io:format("Y ~p~n",[{nodedown,Node}])
 %   after 2000->
%	    io:format("timeout Y ~p~n",[2])
 %   end,
    io:format("sd:all 2 ~p~n",[sd:all()]).    

allocate(App)-> % The calling shall monitor and take actions if node or application dies
    %% Start the needed Node 
    ServiceFile=atom_to_list(App)++".beam",
    ServiceFullFileName=code:where_is_file(ServiceFile),
    ServiceEbinDir=filename:dirname(ServiceFullFileName),
    Cookie=atom_to_list(erlang:get_cookie()),
    %% Infra functions needed [sd
    SdFileName=code:where_is_file("sd.beam"),
    SdEbinDir=filename:dirname(SdFileName),
    % start slave 
    Name =list_to_atom(lists:flatten(io_lib:format("~p",[erlang:system_time()]))),
    {ok,Host}=net:gethostname(),
    Args="-pa "++ServiceEbinDir++" "++"-pa "++SdEbinDir++" "++"-setcookie "++Cookie,
    {ok,Node}=slave:start(Host,Name,Args),
 %   io:format("Node nodes() ~p~n",[{Node,nodes()}]),
    true=net_kernel:connect_node(Node),
    true=erlang:monitor_node(Node,true),
    
     %% Start the gen_server and monitor it instead of using superviosur  
    {ok,PidApp}=rpc:call(Node,App,start,[],5000),
    AppMonitorRef=erlang:monitor(process,PidApp),
    {ok,Node,AppMonitorRef}.

deallocate(Node,App)->
    rpc:call(Node,App,stop,[],2000),
    rpc:call(Node,init,stop,[],1000),
    timer:sleep(100),
    ok.
   
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_3()->

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_4()->
  
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_5()->
  
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

setup()->

  
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
