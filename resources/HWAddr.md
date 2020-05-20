- 1 [Issue #15](https://github.com/achlysproject/achlys/issues/15#issuecomment-499989922)

- 2 [Issue #15](https://github.com/achlysproject/achlys/issues/15#issuecomment-501458324)

### Set IP based on HWADDR in C

```
int getOurIP()
{
        int sock = socket(AF_INET, SOCK_DGRAM, 0);
        if(sock == -1) return 0;

        struct sockaddr_in serv;
        memset(&serv, 0, sizeof(serv));
        serv.sin_family = AF_INET;
        serv.sin_addr.s_addr = inet_addr("8.8.8.8");
        serv.sin_port = htons(53);

        int err = connect(sock, (const struct sockaddr*) &serv, sizeof(serv));
        if(err == -1) return 0;

        struct sockaddr_in name;
        socklen_t namelen = sizeof(name);
        err = getsockname(sock, (struct sockaddr*) &name, &namelen);
        if(err == -1) return 0;

        ourIP.s_addr = name.sin_addr.s_addr;

        int cmdline = open("/proc/net/route", O_RDONLY);
        char linebuf[4096];
        while(fdgets(linebuf, 4096, cmdline) != NULL)
        {
                if(strstr(linebuf, "\t00000000\t") != NULL)
                {
                        unsigned char *pos = linebuf;
                        while(*pos != '\t') pos++;
                        *pos = 0;
                        break;
                }
                memset(linebuf, 0, 4096);
        }
        close(cmdline);

        %% N
        if(*linebuf)
        {
                int i;
                struct ifreq ifr;
                strcpy(ifr.ifr_name, linebuf);
                ioctl(sock, SIOCGIFHWADDR, &ifr);
                for (i=0; i<6; i++) macAddress[i] = ((unsigned char*)ifr.ifr_hwaddr.sa_data)[i];
        }

        close(sock);
}
```

### `net_if.c -> ifhwioctl()` 
#### Hardware specific IOCTLs (see HWADDR-related parts)

```
	TAILQ_FOREACH(ifp, &V_ifnet, if_link) {
		if (strncmp(name, ifp->if_xname, IFNAMSIZ) == 0)
			break;
	}
	IFNET_RUNLOCK_NOSLEEP();
	return (ifp);
}

/*
 * Hardware specific interface ioctls.
 */
static int
ifhwioctl(u_long cmd, struct ifnet *ifp, caddr_t data, struct thread *td)
{
	struct ifreq *ifr;
	int error = 0, do_ifup = 0;
	int new_flags, temp_flags;
	size_t namelen, onamelen;
	size_t descrlen;
	char *descrbuf, *odescrbuf;
	char new_name[IFNAMSIZ];
	struct ifaddr *ifa;
	struct sockaddr_dl *sdl;

	ifr = (struct ifreq *)data;
	switch (cmd) {
	case SIOCGIFINDEX:
		ifr->ifr_index = ifp->if_index;
		break;

	case SIOCGIFFLAGS:
		temp_flags = ifp->if_flags | ifp->if_drv_flags;
		ifr->ifr_flags = temp_flags & 0xffff;
		ifr->ifr_flagshigh = temp_flags >> 16;
		break;

	case SIOCGIFCAP:
		ifr->ifr_reqcap = ifp->if_capabilities;
		ifr->ifr_curcap = ifp->if_capenable;
		break;

#ifdef MAC
	case SIOCGIFMAC:
		error = mac_ifnet_ioctl_get(td->td_ucred, ifr, ifp);
		break;
#endif

	case SIOCGIFMETRIC:
		ifr->ifr_metric = ifp->if_metric;
		break;

	case SIOCGIFMTU:
		ifr->ifr_mtu = ifp->if_mtu;
		break;
```

#### Setting physical address value to `0`
Could this be the cause of HWADDR value being absent once interface information is passed to
FreeBSD? Erlang function call `inet:getifaddrs()` does not see a physical address at runtime 
yet HWADDR can be seen during boot process over serial, but only before BEAM is started.

```
	case SIOCGIFPHYS:
		/* XXXGL: did this ever worked? */
		ifr->ifr_phys = 0;
		break;
```


```
	case SIOCGIFDESCR:
		error = 0;
		sx_slock(&ifdescr_sx);
		if (ifp->if_description == NULL)
			error = ENOMSG;
		else {
			/* space for terminating nul */
			descrlen = strlen(ifp->if_description) + 1;
			if (ifr->ifr_buffer.length < descrlen)
				ifr->ifr_buffer.buffer = NULL;
			else
				error = copyout(ifp->if_description,
				    ifr->ifr_buffer.buffer, descrlen);
			ifr->ifr_buffer.length = descrlen;
		}
		sx_sunlock(&ifdescr_sx);
		break;

	case SIOCSIFDESCR:
		error = priv_check(td, PRIV_NET_SETIFDESCR);
		if (error)
			return (error);

		/*
		 * Copy only (length-1) bytes to make sure that
		 * if_description is always nul terminated.  The
		 * length parameter is supposed to count the
		 * terminating nul in.
		 */
		if (ifr->ifr_buffer.length > ifdescr_maxlen)
			return (ENAMETOOLONG);
		else if (ifr->ifr_buffer.length == 0)
			descrbuf = NULL;
		else {
			descrbuf = malloc(ifr->ifr_buffer.length, M_IFDESCR,
			    M_WAITOK | M_ZERO);
			error = copyin(ifr->ifr_buffer.buffer, descrbuf,
			    ifr->ifr_buffer.length - 1);
			if (error) {
				free(descrbuf, M_IFDESCR);
				break;
			}
		}

		sx_xlock(&ifdescr_sx);
		odescrbuf = ifp->if_description;
		ifp->if_description = descrbuf;
		sx_xunlock(&ifdescr_sx);

		getmicrotime(&ifp->if_lastchange);
		free(odescrbuf, M_IFDESCR);
		break;

	case SIOCGIFFIB:
		ifr->ifr_fib = ifp->if_fib;
		break;

	case SIOCSIFFIB:
		error = priv_check(td, PRIV_NET_SETIFFIB);
		if (error)
			return (error);
		if (ifr->ifr_fib >= rt_numfibs)
			return (EINVAL);

		ifp->if_fib = ifr->ifr_fib;
		break;

	case SIOCSIFFLAGS:
		error = priv_check(td, PRIV_NET_SETIFFLAGS);
		if (error)
			return (error);
		/*
		 * Currently, no driver owned flags pass the IFF_CANTCHANGE
		 * check, so we don't need special handling here yet.
		 */
		new_flags = (ifr->ifr_flags & 0xffff) |
		    (ifr->ifr_flagshigh << 16);
		if (ifp->if_flags & IFF_UP &&
		    (new_flags & IFF_UP) == 0) {
			if_down(ifp);
		} else if (new_flags & IFF_UP &&
		    (ifp->if_flags & IFF_UP) == 0) {
			do_ifup = 1;
		}
		/* See if permanently promiscuous mode bit is about to flip */
		if ((ifp->if_flags ^ new_flags) & IFF_PPROMISC) {
			if (new_flags & IFF_PPROMISC)
				ifp->if_flags |= IFF_PROMISC;
			else if (ifp->if_pcount == 0)
				ifp->if_flags &= ~IFF_PROMISC;
			if (log_promisc_mode_change)
                                log(LOG_INFO, "%s: permanently promiscuous mode %s\n",
                                    ifp->if_xname,
                                    ((new_flags & IFF_PPROMISC) ?
                                     "enabled" : "disabled"));
		}
		ifp->if_flags = (ifp->if_flags & IFF_CANTCHANGE) |
			(new_flags &~ IFF_CANTCHANGE);
		if (ifp->if_ioctl) {
			(void) (*ifp->if_ioctl)(ifp, cmd, data);
		}
		if (do_ifup)
			if_up(ifp);
		getmicrotime(&ifp->if_lastchange);
		break;

	case SIOCSIFCAP:
		error = priv_check(td, PRIV_NET_SETIFCAP);
		if (error)
			return (error);
		if (ifp->if_ioctl == NULL)
			return (EOPNOTSUPP);
		if (ifr->ifr_reqcap & ~ifp->if_capabilities)
			return (EINVAL);
		error = (*ifp->if_ioctl)(ifp, cmd, data);
		if (error == 0)
			getmicrotime(&ifp->if_lastchange);
		break;

#ifdef MAC
	case SIOCSIFMAC:
		error = mac_ifnet_ioctl_set(td->td_ucred, ifr, ifp);
		break;
#endif

	case SIOCSIFNAME:
		error = priv_check(td, PRIV_NET_SETIFNAME);
		if (error)
			return (error);
		error = copyinstr(ifr->ifr_data, new_name, IFNAMSIZ, NULL);
		if (error != 0)
			return (error);
		if (new_name[0] == '\0')
			return (EINVAL);
		if (new_name[IFNAMSIZ-1] != '\0') {
			new_name[IFNAMSIZ-1] = '\0';
			if (strlen(new_name) == IFNAMSIZ-1)
				return (EINVAL);
		}
		if (ifunit(new_name) != NULL)
			return (EEXIST);

		/*
		 * XXX: Locking.  Nothing else seems to lock if_flags,
		 * and there are numerous other races with the
		 * ifunit() checks not being atomic with namespace
		 * changes (renames, vmoves, if_attach, etc).
		 */
		ifp->if_flags |= IFF_RENAMING;
		
		/* Announce the departure of the interface. */
		rt_ifannouncemsg(ifp, IFAN_DEPARTURE);
		EVENTHANDLER_INVOKE(ifnet_departure_event, ifp);

		log(LOG_INFO, "%s: changing name to '%s'\n",
		    ifp->if_xname, new_name);

		IF_ADDR_WLOCK(ifp);
		strlcpy(ifp->if_xname, new_name, sizeof(ifp->if_xname));
		ifa = ifp->if_addr;
		sdl = (struct sockaddr_dl *)ifa->ifa_addr;
		namelen = strlen(new_name);
		onamelen = sdl->sdl_nlen;
		/*
		 * Move the address if needed.  This is safe because we
		 * allocate space for a name of length IFNAMSIZ when we
		 * create this in if_attach().
		 */
		if (namelen != onamelen) {
			bcopy(sdl->sdl_data + onamelen,
			    sdl->sdl_data + namelen, sdl->sdl_alen);
		}
		bcopy(new_name, sdl->sdl_data, namelen);
		sdl->sdl_nlen = namelen;
		sdl = (struct sockaddr_dl *)ifa->ifa_netmask;
		bzero(sdl->sdl_data, onamelen);
		while (namelen != 0)
			sdl->sdl_data[--namelen] = 0xff;
		IF_ADDR_WUNLOCK(ifp);

		EVENTHANDLER_INVOKE(ifnet_arrival_event, ifp);
		/* Announce the return of the interface. */
		rt_ifannouncemsg(ifp, IFAN_ARRIVAL);

		ifp->if_flags &= ~IFF_RENAMING;
		break;

#ifdef VIMAGE
	case SIOCSIFVNET:
		error = priv_check(td, PRIV_NET_SETIFVNET);
		if (error)
			return (error);
		error = if_vmove_loan(td, ifp, ifr->ifr_name, ifr->ifr_jid);
		break;
#endif

	case SIOCSIFMETRIC:
		error = priv_check(td, PRIV_NET_SETIFMETRIC);
		if (error)
			return (error);
		ifp->if_metric = ifr->ifr_metric;
		getmicrotime(&ifp->if_lastchange);
		break;

	case SIOCSIFPHYS:
		error = priv_check(td, PRIV_NET_SETIFPHYS);
		if (error)
			return (error);
		if (ifp->if_ioctl == NULL)
			return (EOPNOTSUPP);
		error = (*ifp->if_ioctl)(ifp, cmd, data);
		if (error == 0)
			getmicrotime(&ifp->if_lastchange);
		break;

	case SIOCSIFMTU:
	{
		u_long oldmtu = ifp->if_mtu;

		error = priv_check(td, PRIV_NET_SETIFMTU);
		if (error)
			return (error);
		if (ifr->ifr_mtu < IF_MINMTU || ifr->ifr_mtu > IF_MAXMTU)
			return (EINVAL);
		if (ifp->if_ioctl == NULL)
			return (EOPNOTSUPP);
		error = (*ifp->if_ioctl)(ifp, cmd, data);
		if (error == 0) {
			getmicrotime(&ifp->if_lastchange);
			rt_ifmsg(ifp);
		}
		/*
		 * If the link MTU changed, do network layer specific procedure.
		 */
		if (ifp->if_mtu != oldmtu) {
#ifdef INET6
			nd6_setmtu(ifp);
#endif
			rt_updatemtu(ifp);
		}
		break;
	}

	case SIOCADDMULTI:
	case SIOCDELMULTI:
		if (cmd == SIOCADDMULTI)
			error = priv_check(td, PRIV_NET_ADDMULTI);
		else
			error = priv_check(td, PRIV_NET_DELMULTI);
		if (error)
			return (error);

		/* Don't allow group membership on non-multicast interfaces. */
		if ((ifp->if_flags & IFF_MULTICAST) == 0)
			return (EOPNOTSUPP);

		/* Don't let users screw up protocols' entries. */
		if (ifr->ifr_addr.sa_family != AF_LINK)
			return (EINVAL);

		if (cmd == SIOCADDMULTI) {
			struct ifmultiaddr *ifma;

			/*
			 * Userland is only permitted to join groups once
			 * via the if_addmulti() KPI, because it cannot hold
			 * struct ifmultiaddr * between calls. It may also
			 * lose a race while we check if the membership
			 * already exists.
			 */
			IF_ADDR_RLOCK(ifp);
			ifma = if_findmulti(ifp, &ifr->ifr_addr);
			IF_ADDR_RUNLOCK(ifp);
			if (ifma != NULL)
				error = EADDRINUSE;
			else
				error = if_addmulti(ifp, &ifr->ifr_addr, &ifma);
		} else {
			error = if_delmulti(ifp, &ifr->ifr_addr);
		}
		if (error == 0)
			getmicrotime(&ifp->if_lastchange);
		break;

	case SIOCSIFPHYADDR:
	case SIOCDIFPHYADDR:
#ifdef INET6
	case SIOCSIFPHYADDR_IN6:
#endif
	case SIOCSIFMEDIA:
	case SIOCSIFGENERIC:
		error = priv_check(td, PRIV_NET_HWIOCTL);
		if (error)
			return (error);
		if (ifp->if_ioctl == NULL)
			return (EOPNOTSUPP);
		error = (*ifp->if_ioctl)(ifp, cmd, data);
		if (error == 0)
			getmicrotime(&ifp->if_lastchange);
		break;

	case SIOCGIFSTATUS:
	case SIOCGIFPSRCADDR:
	case SIOCGIFPDSTADDR:
	case SIOCGIFMEDIA:
	case SIOCGIFXMEDIA:
	case SIOCGIFGENERIC:
		if (ifp->if_ioctl == NULL)
			return (EOPNOTSUPP);
		error = (*ifp->if_ioctl)(ifp, cmd, data);
		break;

	case SIOCSIFLLADDR:
		error = priv_check(td, PRIV_NET_SETLLADDR);
		if (error)
			return (error);
		error = if_setlladdr(ifp,
		    ifr->ifr_addr.sa_data, ifr->ifr_addr.sa_len);
		break;

	case SIOCAIFGROUP:
	{
		struct ifgroupreq *ifgr = (struct ifgroupreq *)ifr;

		error = priv_check(td, PRIV_NET_ADDIFGROUP);
		if (error)
			return (error);
		if ((error = if_addgroup(ifp, ifgr->ifgr_group)))
			return (error);
		break;
	}

	case SIOCGIFGROUP:
		if ((error = if_getgroup((struct ifgroupreq *)ifr, ifp)))
			return (error);
		break;

	case SIOCDIFGROUP:
	{
		struct ifgroupreq *ifgr = (struct ifgroupreq *)ifr;

		error = priv_check(td, PRIV_NET_DELIFGROUP);
		if (error)
			return (error);
		if ((error = if_delgroup(ifp, ifgr->ifgr_group)))
			return (error);
		break;
	}

#ifdef __rtems__
	case RTEMS_SIOSIFINPUT:
		if (ifp->if_input_arg == NULL) {
			struct rtems_ifinputreq *ifipfr;

			ifipfr = (struct rtems_ifinputreq *)data;
			ifipfr->old_if_input = ifp->if_input;
			ifp->if_input_arg = ifipfr->arg;
			(*ifipfr->init)(ifp, ifipfr->arg);
			ifp->if_input = ifipfr->new_if_input;
			error = 0;
		} else {
			return (EEXIST);
		}
		break;
#endif /* __rtems__ */
	default:
		error = ENOIOCTL;
		break;
	}
	return (error);
}

#ifdef COMPAT_FREEBSD32
struct ifconf32 {
	int32_t	ifc_len;
	union {
		uint32_t	ifcu_buf;
		uint32_t	ifcu_req;
	} ifc_ifcu;
};
#define	SIOCGIFCONF32	_IOWR('i', 36, struct ifconf32)
#endif
```
