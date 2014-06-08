%% --------------------------------------------------------------------
%% Logging mechanism
%% Print in standard output
-define(PRINT(Text),
		io:format(Text)).
-define(PRINT(Format, Args),
		io:format(Format, Args)).
-define(TEST_MSG(Format, Args),
		logger:test_msg(?MODULE,?LINE,Format, Args)).
-define(DEBUG(Format, Args),
		logger:debug_msg(?MODULE,?LINE,Format, Args)).
-define(INFO_MSG(Format, Args),
		logger:info_msg(?MODULE,?LINE,Format, Args)).
-define(WARNING_MSG(Format, Args),
		logger:warning_msg(?MODULE,?LINE,Format, Args)).
-define(ERROR_MSG(Format, Args),
		logger:error_msg(?MODULE,?LINE,Format, Args)).
-define(CRITICAL_MSG(Format, Args),
		logger:critical_msg(?MODULE,?LINE,Format, Args)).
