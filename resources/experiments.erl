%%%
%% system_information:sanity_check().
%%
%% Currently the sanity check is limited to verifying runtime
%% dependencies found in the .app files of all applications.
%% This implies that the return type will change in the future.
%% More checks will be introduced in the future.

system_information:sanity_check().
{failed,[{invalid_app_file,acceptor_pool}]}
