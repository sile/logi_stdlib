logi_stdlib
===========

[![hex.pm version](https://img.shields.io/hexpm/v/logi_stdlib.svg)](https://hex.pm/packages/logi_stdlib)

Standard library for [logi](https://github.com/sile/logi)

Build
-----

The build tool is [rebar3](https://github.com/erlang/rebar3).

Add following code to your `rebar.config` file:
```erlang
{deps,
 [
   logi_stdlib
 ]}.
```

Starts erlang shell:
```erlang
$ rebar3 shell

> application:ensure_all_started(logi_stdlib).
> error_logger:tty(false). % Suppresses annoying warnings for the sake of brevity

> {ok, _} = logi_channel:install_sink(logi_sink_console:new(sample), info).
> logi:info("Hello World!").
2016-05-09 13:15:54.758 [info] nonode@nohost <0.88.0> erl_eval:do_apply:674 [] Hello World!
```

API
---

See [EDoc Documents](doc/README.md)

Usage Examples
--------------

### Outputs to console and file

```erlang
%%% Installs sinks
> ConsoleSink = logi_sink_console:new(console).
> FileSink = logi_sink_file:new(file, "/tmp/alert.log").
> {ok, _} = logi_channel:install_sink(ConsoleSink, info).
> {ok, _} = logi_channel:install_sink(FileSink, alert).

%%% Log messages
%% info log
> logi:info("Hello World!").
2016-05-09 13:19:55.327 [info] nonode@nohost <0.88.0> erl_eval:do_apply:674 [] Hello World!

> file:read_file("/tmp/alert.log").
{ok, <<>>}

%% alert log
> logi:alert("Something Wrong").
2016-05-09 13:21:04.953 [alert] nonode@nohost <0.88.0> erl_eval:do_apply:674 [] Something Wrong

> file:read_file("/tmp/alert.log").
{ok,<<"2016-05-09 13:21:04.953 [alert] nonode@nohost <0.88.0> erl_eval:do_apply:674 [] Something Wrong\n">>}
```

### Forwards error_logger messages

```erlang
%%% Installs sinks
> {ok, _} = logi_channel:install_sink(logi_sink_console:new(console), info).

%%% Installs logi's error_logger handler
> ok = logi_source_error_logger:install().

%%% Log messages via error_logger module
> error_logger:info_msg("Hello World!").
2016-05-09 13:28:43.575 [info] nonode@nohost <0.5.0> logi_source_error_logger:default_log_fun:144 [gleader=<0.86.0>,sender=<0.88.0>] Hello World!
```

### Limits log messages size

```erlang
%%% Installs sinks
> Layout = logi_layout_newline:new(logi_layout_limit:new(logi_layout_default:new(), [{max_size, 32}])). % limit: 32 bytes
> Sink = logi_sink_console:new(console, Layout).
> {ok, _} = logi_channel:install_sink(Sink, info).

%%% Log messages
> logi:info("Hello World!").
2016-05-09 13:33:23.840 [info] n...59...  % Tail of the long message was abbreviated
```

### Frequency control per logger

```erlang
%%% Installs sinks
> {ok, _} = logi_channel:install_sink(logi_sink_console:new(console), info).

%%% Setup logger instance with a logi_filter_frequency filter
%%% LIMIT: 3 messages per second
> logi:save_as_default(logi:new([{filter, logi_filter_frequency:new([{max_count, 3}, {period, 1000}])}])).

%%% Log messages
> lists:foreach(fun (I) -> logi:info("Hello: ~p", [I]) end, lists:seq(1, 10)).
2016-05-09 13:41:29.150 [info] nonode@nohost <0.195.0> lists:foreach:1337 [] Hello: 1
2016-05-09 13:41:29.150 [info] nonode@nohost <0.195.0> lists:foreach:1337 [] Hello: 2
2016-05-09 13:41:29.151 [info] nonode@nohost <0.195.0> lists:foreach:1337 [] Hello: 3

> time:sleep(1000).

> logi:info("World!").
2016-05-09 13:41:30.812 [info] nonode@nohost <0.195.0> lists:foreach:1337 [] Over a period of 1.661 seconds, 7 messages were dropped
2016-05-09 13:41:30.812 [info] nonode@nohost <0.195.0> erl_eval:do_apply:674 [] World!
```

### Frequency control per sink

```erlang
%%% Installs sinks
%%% LIMITE: 300 bytes per second
> Sink = logi_sink_flow_limiter:new(limiter, logi_sink_console:new(console), [{write_rate_limits, [{300, 1000}]}]).
> {ok, _} = logi_channel:install_sink(Sink, info).

%%% Log messages
> lists:foreach(fun (I) -> logi:info("Hello: ~p", [I]) end, lists:seq(1, 10)).
2016-05-09 13:48:26.160 [info] nonode@nohost <0.105.0> lists:foreach:1337 [] Hello: 1
2016-05-09 13:48:26.171 [info] nonode@nohost <0.105.0> lists:foreach:1337 [] Hello: 2
2016-05-09 13:48:26.172 [info] nonode@nohost <0.105.0> lists:foreach:1337 [] Hello: 3
2016-05-09 13:48:26.172 [info] nonode@nohost <0.105.0> lists:foreach:1337 [] Hello: 4

> timer:sleep(1000).

> logi:info("World!").
2016-05-09 13:48:43.768 [info] nonode@nohost <0.105.0> erl_eval:do_apply:674 [] World!
2016-05-09 13:49:01.889 [warning] nonode@nohost <0.111.0> logi_sink_flow_limiter_writer:report_omissions:189 [] Over a period of 60 seconds, 6 info messages were omitted: channel=logi_default_log, reason=rate_exceeded (e.g. [{pid,module,line},{<0.105.0>,lists,1337}])
```

### High Availability Configuration

```erlang
%%% Installs sinks
> File1 = logi_sink_file:new(file1, "/tmp/file1.log").
> File2 = logi_sink_file:new(file2, "/tmp/file2.log").
> Sink = logi_sink_ha:new(ha, [#{sink => File1}, #{sink => File2}], [{strategy, first_available}, {logger, null}]).
> {ok, _} = logi_channel:install_sink(Sink, info).

%%% Log messages

%% First available sink is `file1`
> logi:info("Hello").
> file:read_file("/tmp/file1.log").
{ok,<<"2016-05-09 13:55:38.410 [info] nonode@nohost <0.88.0> erl_eval:do_apply:674 [] Hello\n">>}
> file:read_file("/tmp/file2.log").
{ok,<<>>}

%% Removes permission of file1 and forces reopen
> file:change_mode("/tmp/file1.log", 0).
> exit(logi_channel:whereis_sink_proc([ha, file1]), kill).

%% Now, first available sink is `file2`
> logi:info("World!").
> file:read_file("/tmp/file2.log").
{ok,<<"2016-05-09 03:59:06.420 [info] nonode@nohost <0.88.0> erl_eval:do_apply:674 [] World!\n">>}
```

License
-------

This library is released under the MIT License.

See the [LICENSE](LICENSE) file for full license information.
