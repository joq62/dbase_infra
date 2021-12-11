%% This is the application resource file (.app file) for the 'base'
%% application.
{application, dbase_infra,
[{description, "Dbase_Infra application and cluster" },
{vsn, "0.1.0" },
{modules, 
	  [dbase_infra,dbase_infra_sup,dbase_infra_server]},
{registered,[dbase_infra]},
{applications, [kernel,stdlib]},
{mod, {dbase_infra_app,[]}},
{start_phases, []},
{git_path,"https://github.com/joq62/dbase_infra.git"}
]}.
