# Connecting a shell to the cluster of grisp boards via ad-hoc network
If you are in an environment without wifi connection, it can be useful to connect a shell on your computer to the cluster of grisp board nodes.

## Linux

### Creates an ad-hoc network on your computer

Go to `Preferences > Network Connections` then click on `Add new connection`. Select `Wi-Fi` in the list then click on `Create...`.
Give a name to the connection then fill in the fields as follow:

 - General : to be left as it is
 - Wi-Fi :
	 - SSID : `edge`
	 - Mode : `Ad-hoc`
	 - Band : `B/G (2.4 Hhz)`
	 - Channel : 6
	 - Device : leave empty
	 - Cloned MAC address : leave empty
	 - MTU : `Automatic`
 - Wi-Fi Security :
	 - Security : `None`
 - Proxy :
	 - Method : `None`
 - IPv4 Settings
	 - Method : `Link-Local Only`
 - IPv6 Settings :
	 - Method : `Ignore`

Now save the configuration.

### Connects to the new created network
Go to `Preferences > Network` then click on `Connect to Hidden Network..`.
In the field `Connection` select your ad-hoc network then click to `Connect`.

### Retrieves your IPv4
You will need your IPv4 in order to allow your grisp board to connect to your shell. 
In a new shell enter the command `hostname -I | awk '{print $1}'`, the resulting IPv4 must have the form `169.254.X.X`. Keep this IP for the next section.

### Setting correctly your configuration

Modify the following files in the following way :

 - [erl_inetrc](../grisp/grisp_base/files/erl_inetrc) :
	 - In the line `{host, {X,X,X,X}, ["<hostname>"]}.`, replaces `{X,X,X,X}` with your IP : `{169,254,X,X}` and replaces `<hostname>` with the hostname of your PC (you can retrieve your hostname by executing `hostname -s` in a shell).
	 - For each grisp board you want to use in your cluster, enter a similar line and fix its IP and hostname. The IP must have the form `{169,254,X,X}`. We recommend you to not change the existing lines.
 - [sys.config](../config/sys.config) and [test.config.src](../config/test.config.src) :
	 - list all nodes in the cluster except the node for which you are deploying under `boards`
```
{boards, [
     % list of all other nodes of the cluster
     % template: '<sname>@<hostname>'
     'achlys@my_grisp_board_1'
 ]}
```
 - `/etc/hosts` :
	 - add an entry, including the IP and the hostname, for each board of the cluster.

### Deploy

#### GRiSP boards
Deploy with the following command for each GRiSP board (don't forget to change for each board the list of nodes inside the files [sys.config](../config/sys.config) and [test.config.src](../config/test.config.src)).
```
NAME=<hostname> PEER_IP=169,254,X,X IP=169.254.X.X rebar3 grisp deploy -n <application_name> -v <version>
```

#### Shell
Run the following command  (don't forget to change the list of nodes inside the files [sys.config](../config/sys.config) and [test.config.src](../config/test.config.src)). :
```
NAME=<hostname> PEER_PORT=27000 PEER_IP=169,254,X,X IP=169.254.X.X rebar3 as test shell --sname <sname> --setcookie <cookie> --apps <application_name>
```

### Config example
For this example, we have one GRiSP board to which we want to connect with one shell. After setting up correctly our ad-hoc network we have the IP `169.254.126.125`.

#### [erl_inetrc](../grisp/grisp_base/files/erl_inetrc)

```
%--- Add hosts -----------------------------------------------------------------
{host, {169,254,126,125}, ["myhost"]}.
%--- GRiSP boards --------------------------------------------------------------
{host, {169,254,16,1}, ["my_grisp_board_1"]}.
```

#### [sys.config](../config/sys.config) and [test.config.src](../config/test.config.src)

 - When deploying the GRiSP board :
```
{boards, [
     'achlys@myhost'
 ]}
```
 - When deploying the shell
```
{boards, [
     'achlys@my_grisp_board_1'
 ]}
```

#### `/etc/hosts`
```bash
169.254.16.1	my_grisp_board_1
169.254.126.125	myhost
```
#### Deploying the GRiSP
`NAME=my_grisp_board_1 PEER_IP=169,254,16,1 IP=169.254.16.1 rebar3 grisp deploy -n achlys -v 0.3.3`

#### Deploying the shell
`NAME=myhost PEER_PORT=27000 PEER_IP=169,254,126,125 IP=169.254.126.125 rebar3 as test shell --sname achlys --setcookie MyCookie --apps achlys`