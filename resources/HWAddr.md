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
