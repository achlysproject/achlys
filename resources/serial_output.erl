----------------
GRISP bootloader
----------------
Version: bootloader-V1.0.2 (Build Mi 6. Dez 15:09:49 UTC 2017)
nexus0: <RTEMS Nexus device>
at91_mci0: <MCI mmc/sd host bridge> on nexus0
mmc0: <MMC/SD bus> on at91_mci0
media listener: event = DISK ATTACH, state = INQUIRY, src = mmcsd
media listener: event = DISK ATTACH, state = SUCCESS, src = mmcsd, dest = /dev/mmcsd-0
mmedia listener: event = MOUNT, state = INQUIRY, src = /dev/mmcsd-0
mcsd0: 8GB <SDHC SL08G 8.0 SN C5AC10B0 MFG 09/2017 by 3 SD> at mmcmedia listener: event = MOUNT, state = FAILED, src = /dev/mmcsd-0
0media listener: event = PARTITION INQUIRY, state = INQUIRY, src = /dev/mmcsd-0
media listener: event = PARTITION INQUIRY, state = SUCCESS, src = /dev/mmcsd-0
2media listener: event = PARTITION ATTACH, state = INQUIRY, src = /dev/mmcsd-0
0media listener: event = PARTITION ATTACH, state = SUCCESS, src = /dev/mmcsd-0, dest = /dev/mmcsd-0-0
.media listener: event = MOUNT, state = INQUIRY, src = /dev/mmcsd-0-0
5MHz/4bit/1-block
media listener: event = MOUNT, state = SUCCESS, src = /dev/mmcsd-0-0, dest = /media/mmcsd-0-0
[zone: udpcb] kern.ipc.maxsockets limit reached
waiting for SD...

boot: press key to enter service mode
boot: press key to enter service mode
boot: press key to enter service mode
boot: open file "/media/mmcsd-0-0/achlys/erts-10.0/bin/beam.bin"... done
boot: read file "/media/mmcsd-0-0/achlys/erts-10.0/bin/beam.bin"... received 5918492 bytes
mounting sd card
nexus0: <RTEMS Nexus device>
at91_mci0: <MCI mmc/sd host bridge> on nexus0
mmc0: <MMC/SD bus> on at91_mci0
mmcsd0: 8GB <SDHC SL08G 8.0 SN C5AC10B0 MFG 09/2017 by 3 SD> at mmc0 20.5MHz/4bit/1-block
media listener: event = DISK ATTACH, state = INQUIRY, src = mmcsds
af1761otg0: <ISP1761/SAF1761 DCI USB 2.0 Device Controller> on nexus0
media listener: event = DISK ATTACH, state = SUCCESS, src = mmcsd, dest = /dev/mmcsd-0
media listener: event = MOUNT, state = INQUIRY, src = /dev/mmcsd-0
media listener: event = MOUNT, state = FAILED, src = /dev/mmcsd-0
media listener: event = PARTITION INQUIRY, state = INQUIRY, src = /dev/mmcsd-0
media listener: event = PARTITION INQUIRY, state = SUCCESS, src = /dev/mmcsd-0
media listener: event = PARTITION ATTACH, state = INQUIRY, src = /dev/mmcsd-0
media listener: event = PARTITION ATTACH, state = SUCCESS, src = /dev/mmcsd-0, dest = /dev/mmcsd-0-0
media listener: event = MOUNT, state = INQUIRY, src = /dev/mmcsd-0-0
media listener: event = MOUNT, state = SUCCESS, src = /dev/mmcsd-0-0, dest = /media/mmcsd-0-0
usbus0 on saf1761otg0
usbus0: 480Mbps High Speed USB v2.0
ifconfig lo0
waiting for SD...

sd card mounted
ugen0.1: <NXP DCI Root HUB> at usbus0
uhub0: <NXP DCI Root HUB, class 9/0, rev 2.00/1.00, addr 1> on usbus0
grisp.ini: section "boot", name "image_path", value "/media/mmcsd-0-0/achlys/erts-10.0/bin/beam.bin"
grisp.ini: section "erlang", name "args", value "erl.rtems -- -mode embedded -home . -pa . -root achlys -config achlys/releases/0.2.0/sys.config -boot achlys/releases/0.2.0/achlys -internal_epmd epmd_sup -kernel inetrc "./erl_inetrc" -sname achlys -setcookie MyCookie"
erl args: section "erlang", name "args", value "erl.rtems -- -mode embedded -home . -pa . -root achlys -config achlys/releases/0.2.0/sys.config -boot achlys/releases/0.2.0/achlys -internal_epmd epmd_sup -kernel inetrc "./erl_inetrc" -sname achlys -setcookie MyCookie"
grisp.ini: section "network", name "wlan", value "enable"
grisp.ini: section "network", name "ip_self", value "169.254.16.1"
=== Ip is 169.254.16.1 ===
grisp.ini: section "network", name "wlan_ip_netmask", value "255.255.0.0"
grisp.ini: section "network", name "wlan_mode", value "adhoc"
grisp.ini: section "network", name "wlan_adhocname", value "edge"
grisp.ini: section "network", name "wlan_channel", value "6"
grisp.ini: section "network", name "hostname", value "my_grisp_board_1"
erl.rtems -- -mode embedded -home . -pa . -root achlys -config achlys/releases/0.2.0/sys.config -boot achlys/releases/0.2.0/achlys -internal_epmd epmd_sup -kernel inetrc "./erl_inetrc" -sname achlys -setcookie MyCookie
uhub0: 2 ports with 2 removable, self powered
ugen0.2: <Philips Semiconductors ISP1520> at usbus0
uhub1: <Philips Semiconductors ISP1520, class 9/0, rev 2.00/0.00, addr 2> on usbus0
uhub1: 3 ports with 3 removable, self powered
ugen0.3: <Realtek 802.11n NIC> at usbus0
rtwn0 on uhub1
rtwn0: <Realtek 802.11n NIC, class 0/0, rev 2.00/0.00, addr 3> on usbus0
rtwn0: MAC/BB RTL8188EU, RF 6052 1T1R
wlan0: Ethernet address: 38:1d:d9:46:2d:fb
mkdir /tmp
mkdir /tmp/log
mkdir /home
Setting environment
chdir(/media/mmcsd-0-0/)

erl_main: starting ...
getcwd: /media/mmcsd-0-0
hostname: my_grisp_board_1
starting erlang runtime
00:04:09.178 [info] Using node name: achlys@my_grisp_board_1
00:04:09.572 [info] Resolving "my_grisp_board_1"...
00:04:09.583 [info] Resolved "achlys@my_grisp_board_1" to {169,254,16,1}
00:04:09.601 [info] Resolved "my_grisp_board_1" to {169,254,16,1}
00:04:20.965 [info] Partisan listening on {169,254,16,1}:27000 listen_addrs: [#{ip => {169,254,16,1},port => 27000}]
00:04:21.901 [info] Setting jitter: false
00:04:22.261 [info] Setting jitter percent: 1
00:04:22.612 [info] Setting tutorial: false
00:04:22.963 [info] Setting event interval: 0
00:04:23.324 [info] Setting max events: 1000
00:04:23.653 [info] Setting extended logging: false
00:04:23.992 [info] Setting mailbox logging: false
00:04:24.333 [info] Setting operation mode: delta_based
00:04:24.669 [info] Setting set type: orset
00:04:25.350 [info] Setting broadcast: false
00:04:31.183 [info] Membership: false
00:04:31.567 [info] Workflow: false
00:04:32.961 [info] AdClientEnabled: false
00:04:33.648 [info] AdServerEnabled: false
00:04:33.995 [info] TournClientEnabled: false
00:04:34.349 [info] TournServerEnabled: false
00:04:34.684 [info] ThroughputType: gset
00:04:35.036 [info] ThroughputClientEnabled: false
00:04:35.376 [info] ThroughputServerEnabled: false
00:04:35.729 [info] DivergenceType: gcounter
00:04:36.090 [info] DivergenceClientEnabled: false
00:04:36.444 [info] DivergenceServerEnabled: false
00:04:37.422 [info] Backend lasp_dets_storage_backend initialized: achlys@my_grisp_board_1
Eshell V10.0  (abort with ^G)
(achlys@my_grisp_board_1)1> achlys:venom().
ok
(achlys@my_grisp_board_1)2> self().
<0.859.0>
(achlys@my_grisp_board_1)3> erlang:memory().
[{total,17605328},
{processes,2734176},
{processes_used,2731472},
{system,14871152},
{atom,605689},
{atom_used,579286},
{binary,22480},
{code,9272738},
{ets,472736}]
(achlys@my_grisp_board_1)4> achlys:bane_all_preys(temperature).
[{30,20},
{360,20},
{180,20},
{270,20},
{90,20},
{240,20},
{330,20},
{150,20},
{60,20},
{390,20},
{210,20},
{300,20},
{120,20}]
(achlys@my_grisp_board_1)5> achlys:members().
{ok,[achlys@my_grisp_board_1]}

%% whoops :
(achlys@my_grisp_board_1)6> ^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A
^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[[A^[
[A^[[A^[[A^[[A^[[A^[[A^[[A^[[Aerlang:memory().

* 1: syntax error before: '['



(achlys@my_grisp_board_1)6> erlang:memory().

[{total,17604040},

{processes,2639652},

{processes_used,2638976},

{system,14964388},

{atom,605689},

{atom_used,579748},

{binary,23344},

{code,9272738},

{ets,548968}]

(achlys@my_grisp_board_1)7> �254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 20:56:14.854 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],3,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],3,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 20:56:20.154 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],3,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],3,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 20:56:25.334 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port =>
27000}],name => achlys@my_grisp_board_2,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 20:56:30.575 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 20:56:35.697 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],3,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],3,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 20:56:40.940 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],3,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],3,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 20:56:46.051 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name => achlys@my_grisp_board_5,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 20:56:51.238 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],4,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],4,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 20:56:56.388 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name => achlys@my_grisp_board_5,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 20:57:01.533 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],3,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],3,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 21:04:20.886 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 21:04:26.056 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],4,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],4,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 21:04:31.232 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port =>
27000}],name => achlys@my_grisp_board_2,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],1,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],1,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 21:04:36.462 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],3,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],3,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 21:04:41.595 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 21:04:46.766 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],0,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],0,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}

(achlys@my_grisp_board_1)7> 21:04:51.876 [info] Failed to send message:
{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port =>
27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],2,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],2,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 21:04:57.061 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name =>
achlys@my_grisp_board_6,parallelism => 1}],4,#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs
=> [#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels
=> [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name =>
achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,2},port => 27000}],name =>
achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,4},port => 27000}],name =>
achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,5},port => 27000}],name =>
achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs =>
[#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1}],4,
#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue overflow: 8 already 21:05:02.180
[info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},
#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs => [#{ip 
=> {169,254,16,4},port => 27000}]
,name => achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,5},port => 27000}]
,name => achlys@my_grisp_board_5,parallelism => 1},#{channels =>
[1],listen_addrs => [#{ip => {169,254,16,6},port => 27000}]
,name => achlys@my_grisp_board_6,parallelism => 1}],1,#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name =>
achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs => [#{ip => {169,254,16,5},port => 27000}]
,name => achlys@my_grisp_board_5,parallelism => 1} due to
exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs
=> [#{ip => {169,254,16,2},port => 27000}]
,name => achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}]
,name => achlys@my_grisp_board_4,parallelism =>
1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,5},port => 27000}]
,name => achlys@my_grisp_board_5,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,6},port => 27000}]
,name =>
achlys@my_grisp_board_6,parallelism => 1}],1,#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> 21:05:07.297 [info] Failed to send message: {shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}]
,name => achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}]
,name => achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,5},port => 27000}]
,name => achlys@my_grisp_board_5,parallelism => 1}],1,#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name => achlys@my_grisp_board_1,parallelism => 1}} to #{channels => [1],listen_addrs => [#{ip => {169,254,16,5},port => 27000}]
,name => achlys@my_grisp_board_5,parallelism => 1} due to exit:{timeout,{gen_server,call,[<0.9880.1>,{send_message,{shuffle,[#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,2},port => 27000}]
,name => achlys@my_grisp_board_2,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,4},port => 27000}]
,name => achlys@my_grisp_board_4,parallelism => 1},#{channels => [1],listen_addrs => [#{ip => {169,254,16,5},port => 27000}]
,name => achlys@my_grisp_board_5,parallelism => 1}],1,#{channels => [1],listen_addrs => [#{ip => {169,254,16,1},port => 27000}]
,name => achlys@my_grisp_board_1,parallelism => 1}}}]}}
(achlys@my_grisp_board_1)7> debug: _bsd_sonewconn: pcb 0x73e81000: Listen
queue overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreaddebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already idebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already idebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already idebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already indebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreaddebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreaddebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already indebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreaddebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreaddebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreadydebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 alreaddebug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already debug: _bsd_sonewconn: pcb 0x73e81000: Listen queue
overflow: 8 already
