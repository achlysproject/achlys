%%%
%% system_information:sanity_check().
%%
%% Currently the sanity check is limited to verifying runtime
%% dependencies found in the .app files of all applications.
%% This implies that the return type will change in the future.
%% More checks will be introduced in the future.

system_information:sanity_check().
{failed,[{invalid_app_file,acceptor_pool}]}

erl_main: starting ...
getcwd: /media/mmcsd-0-0
hostname: my_grisp_board_1
starting erlang runtime
dets: file "data/achlys@my_grisp_board_1" not properly closed, repairing ...
00:05:14.186 [error] Supervisor lasp_sup had child lasp_distribution_backend started with lasp_distribution_backend:start_link() at undefined exit with reason no match of right hand value {error,{{badmatch,{error,{file_error,{"data/achlys@my_grisp_board_1.TMP","data/achlys@my_grisp_board_1"},eexist}}},[{lasp_dets_storage_backend,start_link,1,[{file,"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_dets_storage_backend.erl"},{line,59}]},{lasp_storage_backend,init,1,[{file,"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_storage_backend.erl"},{line,119}]},{gen_server,init_it,2,[{file,"gen_server.erl"},{line,374}]},{gen_server,init_it,6,[{...},...]},...]}} in lasp_distribution_backend:init/1 line 305 in context start_error
00:05:16.374 [error] CRASH REPORT Process <0.865.0> with 2 neighbours crashed with reason: no match of right hand value {error,{file_error,{"data/achlys@my_grisp_board_1.TMP","data/achlys@my_grisp_board_1"},eexist}} in lasp_dets_storage_backend:start_link/1 line 59
00:05:18.189 [error] CRASH REPORT Process <0.863.0> with 2 neighbours crashed with reason: no match of right hand value {error,{{badmatch,{error,{file_error,{"data/achlys@my_grisp_board_1.TMP","data/achlys@my_grisp_board_1"},eexist}}},[{lasp_dets_storage_backend,start_link,1,[{file,"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_dets_storage_backend.erl"},{line,59}]},{lasp_storage_backend,init,1,[{file,"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_storage_backend.erl"},{line,119}]},{gen_server,init_it,2,[{file,"gen_server.erl"},{line,374}]},{gen_server,init_it,6,[{...},...]},...]}} in lasp_distribution_backend:init/1 line 305
00:05:18.591 [error] CRASH REPORT Process <0.866.0> with 2 neighbours exited with reason: {file_error,{"data/achlys@my_grisp_board_1.TMP","data/achlys@my_grisp_board_1"},eexist} in gen_server:init_it/6 line 358
00:05:21.546 [error] CRASH REPORT Process <0.799.0> with 0 neighbours exited with reason: {{shutdown,{failed_to_start_child,lasp_distribution_backend,{{badmatch,{error,{{badmatch,{error,{file_error,{"data/achlys@my_grisp_board_1.TMP","data/achlys@my_grisp_board_1"},eexist}}},[{lasp_dets_storage_backend,start_link,1,[{file,"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_dets_storage_backend.erl"},{line,59}]},{lasp_storage_backend,init,1,[{file,"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_storage_backend.erl"},{line,119}]},{gen_server,...},...]}}},...}}},...} in application_master:init/4 line 138
{"Kernel pid terminated",application_controller,"{application_start_failure,lasp,{{shutdown,{failed_to_start_child,lasp_distribution_backend,{{badmatch,{error,{{badmatch,{error,{file_error,{\"data/achlys@my_grisp_board_1.TMP\",\"data/achlys@my_grisp_board_1\"},eexist}}},[{lasp_dets_storage_backend,start_link,1,[{file,\"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_dets_storage_backend.erl\"},{line,59}]},{lasp_storage_backend,init,1,[{file,\"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_storage_backend.erl\"},{line,119}]},{gen_server,init_it,2,[{file,\"gen_server.erl\"},{line,374}]},{gen_server,init_it,6,[{file,\"gen_server.erl\"},{line,342}]},{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,249}]}]}}},[{lasp_distribution_backend,init,1,[{file,\"/Users/laymer/EdgeComputing/achlys/_build/default/lib/lasp/src/lasp_distribution_backend.erl\"},{line,305}]},{gen_server,init_it,2,[{file,\"gen_server.erl\"},{line,374}]},{gen_server,init_it,6,[{file,\"gen_server.erl\"},{line,342}]},{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,249}]}]}}},{lasp_app,start,[normal,[]]}}}"}
Kernel pid terminated (application_controller) ({application_start_failure,lasp,{{shutdown,{failed_to_start_child,lasp_distribution_backend,{{badmatch,{error,{{badmatch,{error,{file_error,{"data/achly

Crash dump is being written to: erl_crash.dump...done
atal extension: source=5, is_internal=0, error=1
