## [Troubleshooting Down the Logplex Rabbit Hole](https://blog.heroku.com/logplex-down-the-rabbit-hole#we-solve-this-one-nice-and-easy)

Not knowing what to do, I contacted Lukas Larsson. A few years back, when I spent my first two weeks at Erlang Solutions Ltd. in London, Lukas was also there for a few days and acted as my guide through the city and company. Since then, Lukas has moved on (internally) to consult for the OTP team at Ericsson, and I moved on to AdGear, and then to Heroku. We still connect occasionally at conferences and over IRC, and Lukas has always helped answer my tricky questions about the Erlang VM.

I asked Lukas how I could pinpoint what was wrong—memory leak or fragmentation—and I showed him some of my collected data. I'm sharing what I learned in the process because, in addition to be interesting, the information is not documented anywhere other than the source.

The amount returned by erlang:memory/0-1 is the amount of memory actively allocated, where Erlang terms are laid in memory; this amount does not represent the amount of memory that the OS has given to the virtual machine (and Linux doesn't actually reserve memory pages until they are used by the VM). To understand where memory goes, one must first understand the many allocators being used:
