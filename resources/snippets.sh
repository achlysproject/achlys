sudo screen /dev/tty.usbserial-001901 115200

sudo hostname Igors-MBP

sudo nano /etc/hosts

erl -sname rs -remsh achlys@my_grisp_board_1 -setcookie MyCookie -hidden
erl -sname rs2 -remsh achlys@my_grisp_board_2 -setcookie MyCookie -hidden

networksetup -setmanual "Wi-Fi" 169.254.187.90 255.255.0.0 169.254.187.90

achlys:bite(achlys:declare(rb,all,single,achlys:rainbow())).
achlys:bite(achlys:declare(temp,all,single,achlys:mintemp())).
partisan_default_peer_service_manager:forward_message(achlys@my_grisp_board_1, 1, achlys_load_generator, {stress_msg_in,achlys@my_grisp_board_2}, []).
