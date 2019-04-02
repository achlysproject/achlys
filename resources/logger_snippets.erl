
    %% Synchronous Audit logs
    % {handler, audit_log, logger_disk_log_h,
    %    #{config => #{
    %         file => "logs/audit",
    %         type => wrap,
    %         max_no_files => 50,
    %         max_no_bytes => 52428800, % 50 x 5mb
    %         sync_mode_qlen => 0, % handle all events synchronously
    %         filesync_repeat_interval => 100, % fsync after 100ms when a change is made
    %         %% drop overload control
    %         drop_mode_qlen => 100000000, % ridiculously large queue before we drop
    %         flush_qlen => 100000001, % ensure we pretty much never drop silently
    %         burst_limit_enable => false, % no burst protection
    %         overload_kill_enable => false % kill the whole node, not just the logger
    %      },
    %      % %% only keep what you must
    %      % filters => [
    %      %   {audit_filter, {fun my_project:audit_filter/2, state}}
    %      % ],
    %      filter_default => stop,
    %      %% Keep structured logs to one line
    %      formatter => {logger_formatter , #{single_line => true}}}}
% {kernel , [
%     {logger_level , debug} ,
%     {logger , [
%         {handler,
%             default,
%             logger_disk_log_h,
%             #{config => #{
%                     file => "logs/notice",
%                     type => wrap,
%                     max_no_files => 10,
%                     max_no_bytes => 51200, % 10 x 5kb
%                     burst_limit_enable => true,
%                     burst_limit_max_count => 20,
%                     burst_limit_window_time => 1000,
%                     drop_mode_qlen => 40,
%                     filesync_repeat_interval => 300,
%                     flush_qlen => 60,
%                     overload_kill_enable => false,
%                     overload_kill_mem_size => 3000,
%                     overload_kill_qlen => 200,
%                     overload_kill_restart_after => 15000,
%                     sync_mode_qlen => 10,
%                 },
%             level => notice,
%             formatter => {logger_formatter , #{single_line => true}}}
%         }
%     }
%            ]}
% ]} ,
% {kernel, [my_disk_log_h, logger_disk_log_h,
%                    #{config => #{file => "./my_disk_log",
%                                  type => wrap,
%                                  max_no_files => 4,
%                                  max_no_bytes => 10000},
%                                  filesync_repeat_interval => 1000}}]}.

% {kernel, [
%     {logger_level, notice},
%     {logger, [
%          %% Disk logger for errors
%          {handler, default, logger_disk_log_h,
%             #{config => #{
%                  file => "logs/notice",
%                  type => wrap,
%                  max_no_files => 10,
%                  max_no_bytes => 51200, % 10 x 5mb
%                  burst_limit_enable => true,
%                  burst_limit_max_count => 20,
%                  burst_limit_window_time => 1000,
%                  drop_mode_qlen => 40,
%                  filesync_repeat_interval => 300,
%                  flush_qlen => 60,
%                  overload_kill_enable => false,
%                  overload_kill_mem_size => 3000,
%                  overload_kill_qlen => 200,
%                  overload_kill_restart_after => 15000,
%                  sync_mode_qlen => 10
%             },
%             level => notice,
%             formatter => {flatlog, #{
%                 map_depth => 3,
%                 term_depth => 50}}
%             }
%         }
%     ]}
% ]} ,
% {kernel , [
    % {logger_level , notice} ,
    % {handler,default,logger_disk_log_h,
    % #{config => #{file => "./system_disk_log"}}}
    % {logger , [{handler ,
    %             default ,
    %             logger_disk_log_h ,
    %             #{config => #{ file => "./my_disk_log",type => wrap,max_no_files => 4,max_no_bytes => 10000},filesync_repeat_interval => 1000}
    %             #{level => notice ,
    %               formatter => {logger_formatter , #{single_line => true}}}
    %            }]}
% ]} ,
% {kernel, [my_disk_log_h, logger_disk_log_h,
%                    #{config => #{file => "./my_disk_log",
%                                  type => wrap,
%                                  max_no_files => 4,
%                                  max_no_bytes => 10000},
%                                  filesync_repeat_interval => 1000}}]}.
% {kernel, [
%   {logger_level, notice},
%   {logger_sasl_compatible, true},
%   {logger, [
%     %% Synchronous Audit logs
%     {handler, default, logger_disk_log_h,
%      #{config => #{
%           file => "LOG/disk",
%           type => wrap,
%           max_no_files => 10,
%           max_no_bytes => 51200, % 50 x 5mb
%           sync_mode_qlen => 3, % handle all events synchronously
%           filesync_repeat_interval => 100, % fsync after 100ms when a change is made
%           %% drop overload control
%           drop_mode_qlen => 20, % ridiculously large queue before we drop
%           flush_qlen => 50, % ensure we pretty much never drop silently
%           burst_limit_enable => true, % no burst protection
%           overload_kill_enable => true, % kill the whole node, not just the logger
%
%           burst_limit_max_count => 50,
%           burst_limit_window_time => 1000,
%           overload_kill_mem_size => 30000,
%           overload_kill_qlen => 200,
%           overload_kill_restart_after => 15000
%        },
%        %% only keep what you must
%        % filters => [
%        %   {audit_filter, {fun my_project:audit_filter/2, state}}
%        % ],
%        filter_default => stop,
%        %% Keep structured logs to one line
%        formatter => {flatlog, #{
%          map_depth => 3,
%          term_depth => 50 % bigger term depth
%        }}
%      }
%     }
%     %% Console logger
%     % {handler, console, logger_std_h,
%     %    #{formatter => {flatlog, #{
%     %      map_depth => 3,
%     %      term_depth => 50
%     %     }}}
%     % }
    % Disk logger for errors

%
%
%   ]}
%  ]} ,


% {lager , [
%     {handlers , [
%         {lager_console_backend , [{level , notice}]}
%     ]}
% ]} ,
%
% {logger , [
%     {level , notice}
% ]} ,

% logger:get_config() output :

% #{handlers =>
%       [#{burst_limit_enable => true,burst_limit_max_count => 500,
%          burst_limit_window_time => 1000,
%          config =>
%              #{burst_limit_enable => true,burst_limit_max_count => 500,
%                burst_limit_window_time => 1000,drop_mode_qlen => 200,
%                filesync_repeat_interval => no_repeat,flush_qlen => 1000,
%                overload_kill_enable => false,
%                overload_kill_mem_size => 3000000,
%                overload_kill_qlen => 20000,
%                overload_kill_restart_after => 5000,sync_mode_qlen => 10,
%                type => standard_io},
%          drop_mode_qlen => 200,filesync_repeat_interval => no_repeat,
%          filter_default => log,filters => [],flush_qlen => 1000,
%          formatter => {logger_formatter,#{single_line => true}},
%          id => default,level => notice,module => logger_std_h,
%          overload_kill_enable => false,
%          overload_kill_mem_size => 3000000,
%          overload_kill_qlen => 20000,
%          overload_kill_restart_after => 5000,sync_mode_qlen => 10,
%          type => standard_io}],
%   module_levels => [],
%   primary =>
%       #{filter_default => log,filters => [],level => all}}
% #{handlers =>
%       [
%       #{
%         burst_limit_enable => true,
%         burst_limit_max_count => 50,
%         burst_limit_window_time => 1000,
%         config =>
%         #{
%             burst_limit_enable => true,
%             burst_limit_max_count => 50,
%             burst_limit_window_time => 1000,
%             drop_mode_qlen => 20,
%
%             filesync_repeat_interval => no_repeat,
%             flush_qlen => 50,
%
%             overload_kill_enable => true,
%
%             overload_kill_mem_size => 30000,
%
%             overload_kill_qlen => 200,
%
%             overload_kill_restart_after => 5000,
%             sync_mode_qlen => 3,
%
%             type => standard_io},
%
%          drop_mode_qlen => 20,
%          filesync_repeat_interval => no_repeat,
%          filter_default => log,
%          filters => [],flush_qlen => 50,
%          formatter => {logger_formatter,#{single_line => true}},
%          id => default,level => notice,module => logger_std_h,
%          overload_kill_enable => true,
%          overload_kill_mem_size => 30000,
%          overload_kill_qlen => 200,
%          overload_kill_restart_after => 5000,sync_mode_qlen => 3,
%          type => standard_io}],
%   module_levels => [],
%   primary =>
%       #{filter_default => log,filters => [],level => all}}
