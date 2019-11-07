# Deployment guide

## Environment variables for distribution
Two environment variables can be passed to `rebar3` when deploying an application on a GRiSP board : 

- `IP=X.X.X.X` : where `X.X.X.X` should be replaced by the IPv4 address that will be assigned to the target board.
- `NAME=board_name` : where `board_name` corresponds to the short hostname desired for the node.

A full call to the `deploy` function of the rebar3 Grisp plugin for an Achlys application could look like :

`IP=X.X.X.X NAME=board_name rebar3 grisp deploy -n app_name -v 0.1.0`

# Test shell deployment

A full call to the command below dynamically assigns the correct local IPv4 address to Partisan and the local short name :

`NAME=$(echo hostname -s) PEER_IP=$(ifconfig | grep "inet " | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}' | sed 's/\./,/g') IP=$(ifconfig | grep "inet " | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}') rebar3 as test shell --sname achlys --setcookie MyCookie --apps achlys`

Once the `rebar3 shell` is launched, the Partisan configuration can be verified using :

`(lasp_peer_service:manager()):myself()`