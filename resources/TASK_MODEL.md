# Achlys Task Model Usage

## Scenario
This section describes a scenario using the Achlys Task Model API
to implement a new `gen_server` module that extends Achlys to turn it
into a domain-specific application.

### Task provider module
First, create a new `src/achlys_task_provider` module that implements
the `gen_server` OTP behaviour : 

```
-module(achlys_task_provider).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).

-record(state , {}).

start_link() ->
    gen_server:start_link({local , ?SERVER} , ?MODULE , [] , []).

init([]) ->
    {ok , #state{}}.

handle_call(_Request , _From , State) ->
    {reply , ok , State}.

handle_cast(_Request , State) ->
    {noreply , State}.

handle_info(_Info , State) ->
    {noreply , State}.

terminate(_Reason , _State) ->
    ok.

code_change(_OldVsn , State , _Extra) ->
    {ok , State}.
```

### Initial Task
The server is very polite, therefore it will always greet users
after being started. It will also make all other nodes in the cluster
equally curteous using Achlys. It will add a greeting function to
the working set so that all nodes say hello after a short delay.

Add the function : 

```
schedule_task() ->
    %% Declare an Achlys task that will be
    %% executed exactly once
    Task = achlys:declare(mytask
        , all
        , single
        , fun() ->
             io:format("Hello Joe ! ~n")
    end),
    %% Send the task to the current server module
    %% after a 5000ms delay
    erlang:send_after(5000, ?SERVER, {task, Task}),
    ok.
```

And call it when initializing the task provider :

```
init([]) ->
    ok = schedule_task(),
    {ok , #state{}}.
```

The server will send the message `{task, Task}` to itself
5 seconds after starting. Add a new handler to process this
message accordingly :

```
handle_info({task, Task} , State) ->
    %% Task propagation to the cluster, including self
    achlys:bite(Task),
    {noreply , State};
handle_info(_Info , State) ->
    {noreply , State}.
```

Now the server will add the greeting task in the Achlys Task Model
using the `achlys:bite/1` API call upon receiving this message.

### Temperature conversion
Now that the server behaves nicely, it can begin serving its
actual purpose, that is : 

- 1) Using Achlys to instruct all nodes to gather sensor measurements
from GRiSP boards using the [PmodNAV](https://github.com/grisp/grisp/wiki/PmodNAV-Tutorial) module
- 2) Using a Lasp CRDT set to store and propagate the measurements
from each nodes to all other nodes
- 3) Using Lasp's `lasp:map/3` function to apply a conversion from
Celcius to Farenheit to the PmodNAV temperature measurements. The
expected result is a mirror of the first CRDT set, except that 
temperatures will always converge to their Farenheit equivalent

Add a new function :
```
pmodnav_task() ->
    %% Declare an Achlys task that will be periodically
    %% executed as long as the node is up
    Task = achlys:declare(pmodnav_task
        , all
        , single
        , fun() ->
            logger:log(notice, "Reading PmodNAV measurements ~n"),
            Acc = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl]),
            Gyro = pmod_nav:read(acc, [out_x_g, out_y_g, out_z_g]),
            Mag = pmod_nav:read(mag, [out_x_m, out_y_m, out_z_m]),
            Press = pmod_nav:read(alt, [press_out]),
            Temp = pmod_nav:read(alt, [temp_out]),
            Node = erlang:node(),

            F = fun({Acc, Gyro, Mag, Press, Temp, Node}) ->
                    [T] = Temp,
                    NewTemp = ((T * 1.8) + 32),
                    {Acc, Gyro, Mag, Press, [NewTemp], Node}
            end,
            {ok, {SourceId, _, _, _}} = lasp:declare({<<"source">>, state_orset}, state_orset),
            {ok, {DestinationId, _, _, _}} = lasp:declare({<<"destination">>, state_orset}, state_orset),
            lasp:map(SourceId, F, DestinationId),
            lasp:update(SourceId, {add, {Acc, Gyro, Mag, Press, Temp, Node}}, self())
    end).
```

Extend the server's API :

```
%% Adds the pmodnav_task to the working set
%% using the Achlys task model
-export([add_pmodnav_task/0]).
```

And the implementation that follows :

```
add_pmodnav_task() ->
    gen_server:cast(?SERVER 
        , {task, pmodnav_task()}).
```

You can now handle these incoming messages :

```
handle_cast({task, Task} , State) ->
    %% Task propagation to the cluster, including self
    achlys:bite(Task),    
    {noreply , State};
handle_cast(_Request , State) ->
    {noreply , State}.
```

Done !

### Execution
Testing the provider with 2 nodes can be done locally using
the following sequence of commands : 

```
%% Execution scenario
%% ==================
%%
%% Node 1 shell : 
%% --------------
%%
%% $ make shell n=1 PEER_PORT=27001
%% ...
%% booting up
%% ...
%%
%% (achlys1@130.104.213.164)1> achlys_task_provider:start_link().
%% {ok,<0.806.0>}
%% (achlys1@130.104.213.164)2> Hello Joe !
%% (achlys1@130.104.213.164)2> achlys_task_provider:add_pmodnav_task().
%% ok
%% (achlys1@130.104.213.164)3>
%% (achlys1@130.104.213.164)3> {ok, Set} = lasp:query({<<"source">>, state_orset}), sets:to_list(Set).
%% [{[-1.3732929999999999,-0.789584,-0.23198300000000002],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [42.5],
%%   'achlys1@130.104.213.164'}]
%% (achlys1@130.104.213.164)4>
%% (achlys1@130.104.213.164)4> {ok, FarenheitSet} = lasp:query({<<"destination">>, state_orset}), sets:to_list(FarenheitSet).
%% [{[-1.3732929999999999,-0.789584,-0.23198300000000002],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [108.5],
%%   'achlys1@130.104.213.164'}]
%% (achlys1@130.104.213.164)5>
%%
%% Now start a second Achlys shell : 
%%
%% Node 2 shell : 
%% --------------
%%
%% $ make shell n=2 PEER_PORT=27002
%% ...
%% booting up
%% ...
%%
%% (achlys2@130.104.213.164)1> achlys_util:add_node('achlys1@130.104.213.164').
%% ok
%% (achlys2@130.104.213.164)2> Hello Joe !
%% 
%% (achlys2@130.104.213.164)2>
%% (achlys2@130.104.213.164)2> {ok, FarenheitSet} = lasp:query({<<"destination">>, state_orset}), sets:to_list(FarenheitSet).
%% [{[-1.733376,-1.7716230000000002,0.24387799999999998],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [108.5],
%%   'achlys2@130.104.213.164'},
%%  {[-1.3732929999999999,-0.789584,-0.23198300000000002],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [108.5],
%%   'achlys1@130.104.213.164'}]
%% (achlys2@130.104.213.164)3>
%% (achlys2@130.104.213.164)3> achlys:get_all_tasks().
%% [{#{execution_type => <<1>>,
%%     function => #Fun<achlys_task_provider.0.44631258>,
%%     name => mytask,
%%     targets => <<0>>},
%%   128479609},
%%  {#{execution_type => <<1>>,
%%     function => #Fun<achlys_task_provider.1.44631258>,
%%     name => pmodnav_task,
%%     targets => <<0>>},
%%   30190207}]
%% (achlys2@130.104.213.164)4>
```
