-module(logi_sink_file_path_static).

-hebaviour(logi_sink_file_path).

-export([new/1]).

-export([init/1, next_path/1, handle_info/2]).

new(Path) ->
    logi_sink_file_path:new(?MODULE, Path).

init(Path) ->
    Path.

next_path(Path) ->
    {ok, Path, Path}.

handle_info(_, _) ->
    ignore.
