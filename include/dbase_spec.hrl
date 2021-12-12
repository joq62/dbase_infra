-include("controller.hrl").
-define(DbaseSpec,[{db_host,?HostConfiguration,yes},
		   {db_service_catalog,?ServiceCatalog,yes},
		   {db_deployment,?Deployments,yes},
		   {db_pods,?PodSpecs,yes},
		   {db_deploy_state,na,no},
		   {db_logger,na,no}]).
