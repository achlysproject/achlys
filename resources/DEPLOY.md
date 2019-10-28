# Deployment guide

## Environment variables for distribution
Two environment variables can be passed to `rebar3` when deploying an application on a GRiSP board : 

- `IP=X.X.X.X` : where `X.X.X.X` should be replaced by the IPv4 address that will be assigned to the target board.
- `NAME=board_name` : where `board_name` corresponds to the short hostname desired for the node.
