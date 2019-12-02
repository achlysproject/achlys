# Deployment guide

## Environment variables for distribution
Two environment variables can be passed to `rebar3` when deploying an application on a GRiSP board : 

- `IP=X.X.X.X` : where `X.X.X.X` should be replaced by the IPv4 address that will be assigned to the target board.
- `NAME=board_name` : where `board_name` corresponds to the short hostname desired for the node.

A full call to the `deploy` function of the rebar3 Grisp plugin for an Achlys application could look like :

`IP=X.X.X.X NAME=board_name rebar3 grisp deploy -n app_name -v 0.1.0`

# Test shell deployment

A full call to the command below dynamically assigns the correct local IPv4 address to Partisan and the local short name :

`NAME=$(echo hostname -s) PEER_PORT=27000 PEER_IP=$(ifconfig | grep "inet " | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}' | sed 's/\./,/g') IP=$(ifconfig | grep "inet " | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}') rebar3 as test shell --sname achlys --setcookie MyCookie --apps achlys`

Once the `rebar3 shell` is launched, the Partisan configuration can be verified using :

`(lasp_peer_service:manager()):myself().`

# Running multiple shells locally

It can be useful for development and test purposes to launch several shells
locally. In that case, the `sname` and `peer_port` should be changed to
allow Partisan to use the same IP address and hostname for each local node.

## The `/etc/hosts` file
By default, Unix/Linux-based systems do not contain an entry for each
address the local host gets associated to when connecting to a network.
So trying to clusterize nodes using their hostname will fail each time
the network interface is assigned a different IP address.

### Workaround 1

To overcome this problem in test/debug environments, it is possible
to use `name`s instead of `sname`s i.e. fully qualified names instead
of shortnames : 

- With rebar3 : `rebar3 --sname achlys --setcookie MyCookie` becomes `rebar3 --name 'achlys@X.X.X.X' --setcookie MyCookie`

- In pure Erlang : `erl -sname achlys -setcookie MyCookie` becomes `erl -name 'achlys@X.X.X.X'  -setcookie MyCookie`

with `X.X.X.X` being the current IP address of the host in both cases.

### Workaround 2
If you often use the same IP addresses, for example a home and work/school
address, you can also edit the `/etc/hosts` file directly to add the missing
entries. 

For example, if your computer always uses the address `192.168.1.2` at home,
you can open and terminal and call :

`sudo nano /etc/hosts`

And add 2 lines such as :

```
# Home addr/host pair
192.168.1.2	Hostname

# Work addr/host pair
130.104.222.50	Hostname
```

_NOTE :_ The file is sensitive to subnet masks, and if other entries
are present, the address with the highest subnet mask will always be
used. If several IPs have equally long masks, the *FIRST ONE* is used
as the file is read line by line.

You will now be able to connect nodes between eachother locally using
the short name `Hostname` when it is actually associated with either
of the defined addresses. The final step is to configure the `shell` target
of the `Makefile` accordingly by replacing :

`$(REBAR) as test shell --name '$(GRISPAPP)$(n)@$(IP)'`

with :

`$(REBAR) as test shell --sname $(GRISPAPP)$(n)`

## Launching multiple `rebar3` shells

Open two terminals at the root folder of the project and call :

`make shell n=1 PEER_PORT=27001` in the first one

and then :

`make shell n=2 PEER_PORT=27002` in the second one

You should now have two different Achlys nodes running locally :

- `achlys1@yourhostname`
- `achlys2@yourhostname`

You can clusterize them using the Lasp API that forwards `join` calls to
the Partisan peer service :

- on node `achlys1@yourhostname` :

`lasp_peer_service:join('achlys2@yourhostname').`

_NOTE :_ the single quotes around the node name are required in order to
ensure that even names containing characters such as "-" are still
treated as atoms. For example :

- `lasp_peer_service:join(achlys2@User-Computer-Name).`
- `lasp_peer_service:join(achlys2@192.168.1.1).`

will both throw an error, whereas :

- `lasp_peer_service:join('achlys2@User-Computer-Name').`
- `lasp_peer_service:join('achlys2@192.168.1.1').`

will work correctly.
