-module(logi_sink_file_path).

-callback get_log_file_path(logi_context:context()) -> binary().
