sudo screen /dev/tty.usbserial-001901 115200
sudo screen /dev/tty.usbserial-001911 115200
sudo screen /dev/tty.usbserial-001921 115200
sudo screen /dev/tty.usbserial-001971 115200
sudo screen /dev/tty.usbserial-001981 115200

sudo hostname Igors-MBP

sudo nano /etc/hosts

erl -sname rs -remsh achlys@my_grisp_board_1 -setcookie MyCookie -hidden
erl -sname rs2 -remsh achlys@my_grisp_board_2 -setcookie MyCookie -hidden
erl -sname rs3 -remsh achlys@my_grisp_board_3 -setcookie MyCookie -hidden
erl -sname rs4 -remsh achlys@my_grisp_board_4 -setcookie MyCookie -hidden
erl -sname rs5 -remsh achlys@my_grisp_board_5 -setcookie MyCookie -hidden

networksetup -setmanual "Wi-Fi" 169.254.187.90 255.255.0.0 169.254.187.90

achlys:bite(achlys:declare(rb,all,single,achlys:rainbow())).
achlys:bite(achlys:declare(temp,all,single,achlys:mintemp())).
# partisan_default_peer_service_manager:forward_message(achlys@my_grisp_board_1, 1, achlys_load_generator, {stress_msg_in,achlys@my_grisp_board_2}, []).



# % {resolv_conf, "/etc/resolv.conf"}.

# %--- GRiSP boards --------------------------------------------------------------
# /etc/hosts
# 169.254.187.90	Laymers-MacBook-Pro
# 169.254.16.1	my_grisp_board_1
# 169.254.16.2	my_grisp_board_2
# 169.254.16.3	my_grisp_board_3
# 169.254.16.4	my_grisp_board_4
# 169.254.16.5	my_grisp_board_5
# 169.254.16.6	my_grisp_board_6
# 169.254.16.7	my_grisp_board_7
# 169.254.16.8	my_grisp_board_8
# 169.254.16.9	my_grisp_board_9
# 169.254.16.10	my_grisp_board_10