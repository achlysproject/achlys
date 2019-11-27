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
considered as an atom. For example :

`lasp_peer_service:join(achlys2@User-Computer-Name).`

will not work, whereas :

`lasp_peer_service:join('achlys2@User-Computer-Name').`

will since `'achlys2@User-Computer-Name'`is an atom.