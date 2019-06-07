static void Init(rtems_task_argument arg)
{
  char *argv[] = { "erl.rtems", "--", "-root", "/mnt/otp",
		   "-home", "/mnt/home", "-boot", "uid", 
		   "-noshell", "-noinput",
		   "-config", "/mnt/uid",
		   "-internal_epmd", "epmd_sup", "-sname", "uid" 
		   /* , "-init_debug", "-loader_debug" */
  };
  int argc = sizeof(argv)/sizeof(*argv);

  rtems_status_code sc = RTEMS_SUCCESSFUL;
  int rv = 0;

  sc = rtems_shell_init("SHLL", SHELL_STACK_SIZE, 10,
			CONSOLE_DEVICE_NAME, true, false, NULL);
  assert(sc == RTEMS_SUCCESSFUL);