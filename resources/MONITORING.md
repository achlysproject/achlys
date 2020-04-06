# Measurements

## BASELINE

- `RTEMS SHELL`
- `NO DISTRIBUTION`
- `SAMPLE APPLICATION`

### Sample 1

#### Cpu time

```
Uptime: 2m20.139095          Period: 3.371018
Tasks:   38  Load Average:   77.343%  Load:   26.346%  Idle:   73.653%
Mem:   27M free  30M used   4M stack

 ID         | NAME                | RPRI | CPRI   | TIME                | TOTAL   | CURRENT
------------+---------------------+---------------+---------------------+---------+--^^----
 0x09010001 | IDLE                |  510 |  510   | 31.750971           |  22.656 |  73.653
 0x0a010005 | TIME                |  196 |  196   | 7.844024            |   5.597 |   5.731
 0x0b010004 | (0x70356d91         |  508 |  508   | 1m13.651357         |  52.555 |  14.819
 0x0a010001 | UI1                 |  508 |  508   | 6.259592            |   4.466 |   0.000
 0x0a010014 | CPlt                |   20 |   20   | 2.634317            |   1.879 |   5.698
 0x0a010002 | BSWP                |   30 |   30   | 0.027336            |   0.019 |   0.020
 0x0a010003 | BRDA                |   30 |   30   | 0.498932            |   0.356 |   0.000
 0x0a010006 | IRQS                |  192 |  192   | 0.191685            |   0.136 |   0.028
 0x0a010011 | _BSD                |  200 |  200   | 0.198269            |   0.141 |   0.030
 0x0a010010 | _BSD                |  200 |  200   | 0.129475            |   0.092 |   0.018
 0x0a010004 | MDIA                |  400 |  400   | 0.043811            |   0.031 |   0.000
 0x0a010007 | _BSD                |  200 |  200   | 0.000034            |   0.000 |   0.000
 0x0a010008 | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a010009 | _BSD                |  200 |  200   | 0.000048            |   0.000 |   0.000
 0x0a01000a | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000b | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000c | _BSD                |  200 |  200   | 0.000039            |   0.000 |   0.000
 0x0a01000d | _BSD                |  200 |  200   | 0.000041            |   0.000 |   0.000
 0x0a01000e | _BSD                |  200 |  200   | 0.000381            |   0.000 |   0.000
 0x0a01000f | _BSD                |  200 |  200   | 0.000038            |   0.000 |   0.000
 0x0a010012 | SHLL                |   20 |   20   | 0.157584            |   0.112 |   0.000
 0x0a010013 | _BSD                |  200 |  200   | 0.000056            |   0.000 |   0.000
 0x0b010001 | (0x70356d91         |  508 |  508   | 0.000397            |   0.000 |   0.000
 0x0b010002 | (0x70356d91         |  508 |  508   | 0.000536            |   0.000 |   0.000
 0x0b010003 | (0x70356d91         |  508 |  508   | 0.000491            |   0.000 |   0.000
 0x0b010005 | (0x70356d91         |  508 |  508   | 0.014942            |   0.010 |   0.000
 0x0b010006 | (0x70356d91         |  508 |  508   | 1.751878            |   1.250 |   0.000
 0x0b010007 | (0x70356d91         |  508 |  508   | 1.668641            |   1.190 |   0.000
 0x0b010008 | (0x70356d91         |  508 |  508   | 1.367827            |   0.976 |   0.000
 0x0b010009 | (0x70356d91         |  508 |  508   | 1.474391            |   1.052 |   0.000
 0x0b01000a | (0x70356d91         |  508 |  508   | 1.639087            |   1.169 |   0.000
 0x0b01000b | (0x70356d91         |  508 |  508   | 1.372425            |   0.979 |   0.000
 0x0b01000c | (0x70356d91         |  508 |  508   | 1.632969            |   1.165 |   0.000
 0x0b01000d | (0x70356d91         |  508 |  508   | 1.552016            |   1.107 |   0.000
 0x0b01000e | (0x70356d91         |  508 |  508   | 1.431627            |   1.021 |   0.000
 0x0b01000f | (0x70356d91         |  508 |  508   | 1.855835            |   1.324 |   0.000
 0x0b010010 | (0x70356d91         |  508 |  508   | 0.987635            |   0.704 |   0.000
 0x0b010011 | (0x70356d91         |  508 |  508   | 0.000695            |   0.000 |   0.000
load monitoring stopped.
```

#### Memory allocations

```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                              37
Largest free block:                           10909800
Total bytes free:                             28768632
Number of used blocks:                            1023
Largest used block:                            8388616
Total bytes used:                             31762048
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              20445560
Maximum number of free blocks ever:                 41
Maximum number of blocks searched ever:              7
Lifetime number of bytes allocated:           48573984
Lifetime number of bytes freed:               16827448
Total number of searches:                         6048
Total number of successful allocations:           5750
Total number of failed allocations:                  0
Total number of successful frees:                 4727
Total number of successful resizes:                 25
SHLL [/] #
```

### Sample 2

#### Cpu time

```
Uptime: 5m10.918079          Period: 3.371000
Tasks:   38  Load Average:   45.322%  Load:   26.700%  Idle:   73.297%
Mem:   27M free  30M used   4M stack

 ID         | NAME                | RPRI | CPRI   | TIME                | TOTAL   | CURRENT
------------+---------------------+---------------+---------------------+---------+--^^----
 0x09010001 | IDLE                |  510 |  510   | 2m47.177077         |  54.261 |  73.297
 0x0a010005 | TIME                |  196 |  196   | 17.338177           |   5.627 |   5.715
 0x0b010004 | (0x70356d91         |  508 |  508   | 1m38.208234         |  31.876 |  15.263
 0x0a010001 | UI1                 |  508 |  508   | 6.259592            |   2.031 |   0.000
 0x0a010014 | CPlt                |   20 |   20   | 0.904715            |   0.293 |   5.701
 0x0a010002 | BSWP                |   30 |   30   | 0.060519            |   0.019 |   0.020
 0x0a010003 | BRDA                |   30 |   30   | 0.498932            |   0.161 |   0.000
 0x0a010004 | MDIA                |  400 |  400   | 0.043811            |   0.014 |   0.000
 0x0a010006 | IRQS                |  192 |  192   | 0.239784            |   0.077 |   0.000
 0x0a010007 | _BSD                |  200 |  200   | 0.000034            |   0.000 |   0.000
 0x0a010008 | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a010009 | _BSD                |  200 |  200   | 0.000048            |   0.000 |   0.000
 0x0a01000a | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000b | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000c | _BSD                |  200 |  200   | 0.000039            |   0.000 |   0.000
 0x0a01000d | _BSD                |  200 |  200   | 0.000041            |   0.000 |   0.000
 0x0a01000e | _BSD                |  200 |  200   | 0.000381            |   0.000 |   0.000
 0x0a01000f | _BSD                |  200 |  200   | 0.000038            |   0.000 |   0.000
 0x0a010010 | _BSD                |  200 |  200   | 0.156789            |   0.050 |   0.000
 0x0a010011 | _BSD                |  200 |  200   | 0.242999            |   0.078 |   0.000
 0x0a010012 | SHLL                |   20 |   20   | 0.209996            |   0.068 |   0.000
 0x0a010013 | _BSD                |  200 |  200   | 0.000056            |   0.000 |   0.000
 0x0b010001 | (0x70356d91         |  508 |  508   | 0.000397            |   0.000 |   0.000
 0x0b010002 | (0x70356d91         |  508 |  508   | 0.000536            |   0.000 |   0.000
 0x0b010003 | (0x70356d91         |  508 |  508   | 0.000491            |   0.000 |   0.000
 0x0b010005 | (0x70356d91         |  508 |  508   | 0.014942            |   0.004 |   0.000
 0x0b010006 | (0x70356d91         |  508 |  508   | 1.751878            |   0.568 |   0.000
 0x0b010007 | (0x70356d91         |  508 |  508   | 1.668641            |   0.541 |   0.000
 0x0b010008 | (0x70356d91         |  508 |  508   | 1.367827            |   0.443 |   0.000
 0x0b010009 | (0x70356d91         |  508 |  508   | 1.474391            |   0.478 |   0.000
 0x0b01000a | (0x70356d91         |  508 |  508   | 1.639087            |   0.532 |   0.000
 0x0b01000b | (0x70356d91         |  508 |  508   | 1.372425            |   0.445 |   0.000
 0x0b01000c | (0x70356d91         |  508 |  508   | 1.632969            |   0.530 |   0.000
 0x0b01000d | (0x70356d91         |  508 |  508   | 1.552016            |   0.503 |   0.000
 0x0b01000e | (0x70356d91         |  508 |  508   | 1.431627            |   0.464 |   0.000
 0x0b01000f | (0x70356d91         |  508 |  508   | 1.855835            |   0.602 |   0.000
 0x0b010010 | (0x70356d91         |  508 |  508   | 0.987635            |   0.320 |   0.000
 0x0b010011 | (0x70356d91         |  508 |  508   | 0.000695            |   0.000 |   0.000
load monitoring stopped.
```

#### Memory allocations

```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                              38
Largest free block:                           10843768
Total bytes free:                             28768312
Number of used blocks:                            1024
Largest used block:                            8388616
Total bytes used:                             31762368
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              20445560
Maximum number of free blocks ever:                 41
Maximum number of blocks searched ever:              7
Lifetime number of bytes allocated:           48643080
Lifetime number of bytes freed:               16896224
Total number of searches:                         6068
Total number of successful allocations:           5770
Total number of failed allocations:                  0
Total number of successful frees:                 4746
Total number of successful resizes:                 25
SHLL [/] #
```

## Simple Erlang Node

- `RTEMS SHELL`
- `ERLANG DISTRIBUTION`
- `SAMPLE APPLICATION`
- `SINGLE NODE`

### Sample 1

#### Cpu time

```
Uptime: 33.849247            Period: 2.718155
Tasks:   38  Load Average:   81.123%  Load:  100.002%  Idle:    0.000%
Mem:   28M free  29M used   4M stack

 ID         | NAME                | RPRI | CPRI   | TIME                | TOTAL   | CURRENT
------------+---------------------+---------------+---------------------+---------+--^^----
 0x0b010004 | (0x70356d91         |  508 |  508   | 11.051034           |  32.647 |  71.443
 0x09010001 | IDLE                |  510 |  510   | 6.390015            |  18.877 |   0.000
 0x0a010001 | UI1                 |  508 |  508   | 7.084517            |  20.929 |   0.000
 0x0a010003 | BRDA                |   30 |   30   | 0.411726            |   1.216 |   0.378
 0x0a010014 | CPlt                |   20 |   20   | 0.136946            |   0.404 |   4.985
 0x0a010002 | BSWP                |   30 |   30   | 0.005381            |   0.015 |   0.022
 0x0a010006 | IRQS                |  192 |  192   | 1.738155            |   5.134 |   6.504
 0x0a010005 | TIME                |  196 |  196   | 1.560740            |   4.610 |   6.227
 0x0a010011 | _BSD                |  200 |  200   | 1.497281            |   4.423 |   4.787
 0x0a01000f | _BSD                |  200 |  200   | 0.479371            |   1.416 |   2.687
 0x0a010012 | _BSD                |  200 |  200   | 0.188368            |   0.556 |   0.035
 0x0a010013 | SHLL                |   20 |   20   | 0.062582            |   0.184 |   0.003
 0x0a010004 | MDIA                |  400 |  400   | 0.043871            |   0.129 |   0.000
 0x0a010007 | _BSD                |  200 |  200   | 0.000034            |   0.000 |   0.000
 0x0a010008 | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a010009 | _BSD                |  200 |  200   | 0.000048            |   0.000 |   0.000
 0x0a01000a | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000b | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000c | _BSD                |  200 |  200   | 0.000040            |   0.000 |   0.000
 0x0a01000d | _BSD                |  200 |  200   | 0.000042            |   0.000 |   0.000
 0x0a01000e | _BSD                |  200 |  200   | 0.000352            |   0.001 |   0.000
 0x0a010010 | _BSD                |  200 |  200   | 0.109589            |   0.323 |   0.000
 0x0b01000a | (0x70356d91         |  508 |  508   | 0.321982            |   0.951 |   0.578
 0x0b01000c | (0x70356d91         |  508 |  508   | 0.350248            |   1.034 |   0.555
 0x0b010007 | (0x70356d91         |  508 |  508   | 0.313218            |   0.925 |   0.527
 0x0b010006 | (0x70356d91         |  508 |  508   | 0.243450            |   0.719 |   0.031
 0x0b01000b | (0x70356d91         |  508 |  508   | 0.222890            |   0.658 |   0.554
 0x0b010008 | (0x70356d91         |  508 |  508   | 0.197343            |   0.582 |   0.172
 0x0b01000f | (0x70356d91         |  508 |  508   | 0.326468            |   0.964 |   0.021
 0x0b01000e | (0x70356d91         |  508 |  508   | 0.325088            |   0.960 |   0.021
 0x0b01000d | (0x70356d91         |  508 |  508   | 0.304989            |   0.901 |   0.017
 0x0b010010 | (0x70356d91         |  508 |  508   | 0.137300            |   0.405 |   0.447
 0x0b010001 | (0x70356d91         |  508 |  508   | 0.000376            |   0.001 |   0.000
 0x0b010002 | (0x70356d91         |  508 |  508   | 0.000501            |   0.001 |   0.000
 0x0b010003 | (0x70356d91         |  508 |  508   | 0.000570            |   0.001 |   0.000
 0x0b010005 | (0x70356d91         |  508 |  508   | 0.015534            |   0.045 |   0.000
 0x0b010009 | (0x70356d91         |  508 |  508   | 0.328820            |   0.971 |   0.000
 0x0b010011 | (0x70356d91         |  508 |  508   | 0.000724            |   0.002 |   0.000
load monitoring stopped.
```

#### Memory allocations

```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             150
Largest free block:                           27956352
Total bytes free:                             31924768
Number of used blocks:                            2070
Largest used block:                            8388616
Total bytes used:                             28605912
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              31924592
Maximum number of free blocks ever:                153
Maximum number of blocks searched ever:            125
Lifetime number of bytes allocated:           29869016
Lifetime number of bytes freed:                1266584
Total number of searches:                        24019
Total number of successful allocations:           6691
Total number of failed allocations:                  0
Total number of successful frees:                 4621
Total number of successful resizes:                 18
```

### Sample 2

#### Cpu time

```
Uptime: 4m17.967080          Period: 3.374000
Tasks:   38  Load Average:   64.113%  Load:   38.378%  Idle:   61.621%
Mem:   27M free  30M used   4M stack

 ID         | NAME                | RPRI | CPRI   | TIME                | TOTAL   | CURRENT
------------+---------------------+---------------+---------------------+---------+--^^----
 0x09010001 | IDLE                |  510 |  510   | 1m31.654643         |  35.656 |  61.621
 0x0a010006 | IRQS                |  192 |  192   | 17.296626           |   6.728 |   6.524
 0x0a010005 | TIME                |  196 |  196   | 15.165959           |   5.900 |   5.966
 0x0a010011 | _BSD                |  200 |  200   | 13.114504           |   5.102 |   4.429
 0x0b010004 | (0x70356d91         |  508 |  508   | 1m23.891313         |  32.636 |  13.044
 0x0a010001 | UI1                 |  508 |  508   | 7.084517            |   2.756 |   0.000
 0x0a010014 | CPlt                |   20 |   20   | 1.515504            |   0.589 |   5.854
 0x0a010002 | BSWP                |   30 |   30   | 0.051351            |   0.019 |   0.023
 0x0a010003 | BRDA                |   30 |   30   | 1.834826            |   0.713 |   0.000
 0x0a01000f | _BSD                |  200 |  200   | 6.853286            |   2.666 |   2.459
 0x0a010012 | _BSD                |  200 |  200   | 0.260470            |   0.101 |   0.058
 0x0a010010 | _BSD                |  200 |  200   | 0.145305            |   0.056 |   0.017
 0x0a010004 | MDIA                |  400 |  400   | 0.043871            |   0.017 |   0.000
 0x0a010007 | _BSD                |  200 |  200   | 0.002235            |   0.000 |   0.000
 0x0a010008 | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a010009 | _BSD                |  200 |  200   | 0.000048            |   0.000 |   0.000
 0x0a01000a | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000b | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000c | _BSD                |  200 |  200   | 0.000040            |   0.000 |   0.000
 0x0a01000d | _BSD                |  200 |  200   | 0.000042            |   0.000 |   0.000
 0x0a01000e | _BSD                |  200 |  200   | 0.000352            |   0.000 |   0.000
 0x0a010013 | SHLL                |   20 |   20   | 0.116801            |   0.045 |   0.000
 0x0b010001 | (0x70356d91         |  508 |  508   | 0.000376            |   0.000 |   0.000
 0x0b010002 | (0x70356d91         |  508 |  508   | 0.000501            |   0.000 |   0.000
 0x0b010003 | (0x70356d91         |  508 |  508   | 0.000570            |   0.000 |   0.000
 0x0b010005 | (0x70356d91         |  508 |  508   | 0.015534            |   0.006 |   0.000
 0x0b010006 | (0x70356d91         |  508 |  508   | 1.897009            |   0.738 |   0.000
 0x0b010007 | (0x70356d91         |  508 |  508   | 1.968996            |   0.766 |   0.000
 0x0b010008 | (0x70356d91         |  508 |  508   | 1.443717            |   0.561 |   0.000
 0x0b010009 | (0x70356d91         |  508 |  508   | 1.722432            |   0.670 |   0.000
 0x0b01000a | (0x70356d91         |  508 |  508   | 1.700133            |   0.661 |   0.000
 0x0b01000b | (0x70356d91         |  508 |  508   | 1.415260            |   0.550 |   0.000
 0x0b01000c | (0x70356d91         |  508 |  508   | 1.778699            |   0.691 |   0.000
 0x0b01000d | (0x70356d91         |  508 |  508   | 1.601342            |   0.622 |   0.000
 0x0b01000e | (0x70356d91         |  508 |  508   | 1.653617            |   0.643 |   0.000
 0x0b01000f | (0x70356d91         |  508 |  508   | 1.846779            |   0.718 |   0.000
 0x0b010010 | (0x70356d91         |  508 |  508   | 0.967829            |   0.376 |   0.000
 0x0b010011 | (0x70356d91         |  508 |  508   | 0.001750            |   0.000 |   0.000
load monitoring stopped.
```

#### Memory allocations

```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             161
Largest free block:                           10783752
Total bytes free:                             28712456
Number of used blocks:                            2080
Largest used block:                            8388616
Total bytes used:                             31818224
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              21372792
Maximum number of free blocks ever:                162
Maximum number of blocks searched ever:            125
Lifetime number of bytes allocated:           62935656
Lifetime number of bytes freed:               31135136
Total number of searches:                        36622
Total number of successful allocations:          18129
Total number of failed allocations:                  0
Total number of successful frees:                16049
Total number of successful resizes:                 32
```

### Sample 3

#### Cpu time

```
Uptime: 35m27.380086         Period: 3.374001
Tasks:   38  Load Average:   43.237%  Load:   44.097%  Idle:   55.902%
Mem:   27M free  30M used   4M stack

 ID         | NAME                | RPRI | CPRI   | TIME                | TOTAL   | CURRENT
------------+---------------------+---------------+---------------------+---------+--^^----
 0x09010001 | IDLE                |  510 |  510   | 20m4.918333         |  56.708 |  55.902
 0x0a010006 | IRQS                |  192 |  192   | 3m11.827512         |   9.028 |   9.132
 0x0a010005 | TIME                |  196 |  196   | 2m7.719692          |   6.011 |   6.024
 0x0a010011 | _BSD                |  200 |  200   | 2m27.243868         |   6.929 |   6.677
 0x0a01000f | _BSD                |  200 |  200   | 1m19.897158         |   3.760 |   3.811
 0x0b010004 | (0x70356d91         |  508 |  508   | 5m42.850652         |  16.136 |  12.537
 0x0a010001 | UI1                 |  508 |  508   | 7.084517            |   0.333 |   0.000
 0x0a010014 | CPlt                |   20 |   20   | 1.319534            |   0.062 |   5.838
 0x0a010002 | BSWP                |   30 |   30   | 0.423557            |   0.019 |   0.020
 0x0a010003 | BRDA                |   30 |   30   | 1.834826            |   0.086 |   0.000
 0x0a010012 | _BSD                |  200 |  200   | 0.860369            |   0.040 |   0.035
 0x0a010010 | _BSD                |  200 |  200   | 0.434001            |   0.020 |   0.018
 0x0a010004 | MDIA                |  400 |  400   | 0.043871            |   0.002 |   0.000
 0x0a010007 | _BSD                |  200 |  200   | 0.002235            |   0.000 |   0.000
 0x0a010008 | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a010009 | _BSD                |  200 |  200   | 0.000048            |   0.000 |   0.000
 0x0a01000a | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000b | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000c | _BSD                |  200 |  200   | 0.000040            |   0.000 |   0.000
 0x0a01000d | _BSD                |  200 |  200   | 0.000042            |   0.000 |   0.000
 0x0a01000e | _BSD                |  200 |  200   | 0.000352            |   0.000 |   0.000
 0x0a010013 | SHLL                |   20 |   20   | 0.271119            |   0.012 |   0.000
 0x0b010001 | (0x70356d91         |  508 |  508   | 0.000376            |   0.000 |   0.000
 0x0b010002 | (0x70356d91         |  508 |  508   | 0.000501            |   0.000 |   0.000
 0x0b010003 | (0x70356d91         |  508 |  508   | 0.000570            |   0.000 |   0.000
 0x0b010005 | (0x70356d91         |  508 |  508   | 0.015534            |   0.000 |   0.000
 0x0b010006 | (0x70356d91         |  508 |  508   | 1.897009            |   0.089 |   0.000
 0x0b010007 | (0x70356d91         |  508 |  508   | 1.968996            |   0.092 |   0.000
 0x0b010008 | (0x70356d91         |  508 |  508   | 1.443717            |   0.067 |   0.000
 0x0b010009 | (0x70356d91         |  508 |  508   | 1.722432            |   0.081 |   0.000
 0x0b01000a | (0x70356d91         |  508 |  508   | 1.700133            |   0.080 |   0.000
 0x0b01000b | (0x70356d91         |  508 |  508   | 1.415260            |   0.066 |   0.000
 0x0b01000c | (0x70356d91         |  508 |  508   | 1.778699            |   0.083 |   0.000
 0x0b01000d | (0x70356d91         |  508 |  508   | 1.601342            |   0.075 |   0.000
 0x0b01000e | (0x70356d91         |  508 |  508   | 1.653617            |   0.077 |   0.000
 0x0b01000f | (0x70356d91         |  508 |  508   | 1.846779            |   0.086 |   0.000
 0x0b010010 | (0x70356d91         |  508 |  508   | 0.967829            |   0.045 |   0.000
 0x0b010011 | (0x70356d91         |  508 |  508   | 0.001750            |   0.000 |   0.000
load monitoring stopped.
```

#### Memory allocations

```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             163
Largest free block:                           10717568
Total bytes free:                             28712136
Number of used blocks:                            2081
Largest used block:                            8388616
Total bytes used:                             31818544
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              21372792
Maximum number of free blocks ever:                164
Maximum number of blocks searched ever:            125
Lifetime number of bytes allocated:           74576416
Lifetime number of bytes freed:               42775576
Total number of searches:                       112860
Total number of successful allocations:          94367
Total number of failed allocations:                  0
Total number of successful frees:                92286
Total number of successful resizes:                 32
SHLL [/] #
```


## Simple Erlang Node

- `RTEMS SHELL`
- `ERLANG DISTRIBUTION`
- `SAMPLE APPLICATION`
- `7 NODES`

### Sample 1

#### Cpu time

```
Uptime: 8hr6m44.887188       Period: 3.373991
Tasks:   38  Load Average:   33.858%  Load:   42.113%  Idle:   57.886%
Mem:   27M free  30M used   4M stack

 ID         | NAME                | RPRI | CPRI   | TIME                | TOTAL   | CURRENT
------------+---------------------+---------------+---------------------+---------+--^^----
 0x09010001 | IDLE                |  510 |  510   | 5hr21m56.675340     |  66.141 |  57.886
 0x0a010005 | TIME                |  196 |  196   | 30m49.493878        |   6.332 |   6.640
 0x0a010006 | IRQS                |  192 |  192   | 28m43.912157        |   5.902 |   7.641
 0x0a010011 | _BSD                |  200 |  200   | 21m58.152305        |   4.513 |   5.226
 0x0a01000f | _BSD                |  200 |  200   | 12m19.210701        |   2.531 |   2.978
 0x0a010012 | _BSD                |  200 |  200   | 9.441478            |   0.032 |   0.042
 0x0b010004 | (0x70356d91         |  508 |  508   | 1hr10m8.776402      |  14.411 |  13.781
 0x0a010001 | UI1                 |  508 |  508   | 7.084322            |   0.024 |   0.000
 0x0a010002 | BSWP                |   30 |   30   | 5.718421            |   0.019 |   0.024
 0x0a010003 | BRDA                |   30 |   30   | 1.832150            |   0.006 |   0.000
 0x0a010010 | _BSD                |  200 |  200   | 4.423964            |   0.015 |   0.027
 0x0a010014 | CPlt                |   20 |   20   | 0.717620            |   0.002 |   5.751
 0x0a010004 | MDIA                |  400 |  400   | 0.043814            |   0.000 |   0.000
 0x0a010007 | _BSD                |  200 |  200   | 0.004136            |   0.000 |   0.000
 0x0a010008 | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a010009 | _BSD                |  200 |  200   | 0.000047            |   0.000 |   0.000
 0x0a01000a | _BSD                |  200 |  200   | 0.000029            |   0.000 |   0.000
 0x0a01000b | _BSD                |  200 |  200   | 0.000028            |   0.000 |   0.000
 0x0a01000c | _BSD                |  200 |  200   | 0.000040            |   0.000 |   0.000
 0x0a01000d | _BSD                |  200 |  200   | 0.000041            |   0.000 |   0.000
 0x0a01000e | _BSD                |  200 |  200   | 0.000333            |   0.000 |   0.000
 0x0a010013 | SHLL                |   20 |   20   | 0.011573            |   0.000 |   0.000
 0x0b010001 | (0x70356d91         |  508 |  508   | 0.000341            |   0.000 |   0.000
 0x0b010002 | (0x70356d91         |  508 |  508   | 0.010116            |   0.000 |   0.000
 0x0b010003 | (0x70356d91         |  508 |  508   | 1.116552            |   0.003 |   0.000
 0x0b010005 | (0x70356d91         |  508 |  508   | 0.015230            |   0.000 |   0.000
 0x0b010006 | (0x70356d91         |  508 |  508   | 1.750726            |   0.005 |   0.000
 0x0b010007 | (0x70356d91         |  508 |  508   | 2.326566            |   0.007 |   0.000
 0x0b010008 | (0x70356d91         |  508 |  508   | 1.472661            |   0.005 |   0.000
 0x0b010009 | (0x70356d91         |  508 |  508   | 1.746431            |   0.005 |   0.000
 0x0b01000a | (0x70356d91         |  508 |  508   | 1.519084            |   0.005 |   0.000
 0x0b01000b | (0x70356d91         |  508 |  508   | 1.639252            |   0.005 |   0.000
 0x0b01000c | (0x70356d91         |  508 |  508   | 1.705636            |   0.005 |   0.000
 0x0b01000d | (0x70356d91         |  508 |  508   | 1.687527            |   0.005 |   0.000
 0x0b01000e | (0x70356d91         |  508 |  508   | 1.664351            |   0.005 |   0.000
 0x0b01000f | (0x70356d91         |  508 |  508   | 1.610474            |   0.005 |   0.000
 0x0b010010 | (0x70356d91         |  508 |  508   | 1.014071            |   0.003 |   0.000
 0x0b010011 | (0x70356d91         |  508 |  508   | 0.109750            |   0.000 |   0.000
load monitoring stopped.
```

#### Memory allocations


```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             170
Largest free block:                           10793056
Total bytes free:                             28658336
Number of used blocks:                            2114
Largest used block:                            8388616
Total bytes used:                             31872344
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              21404312
Maximum number of free blocks ever:                171
Maximum number of blocks searched ever:            125
Lifetime number of bytes allocated:          177875880
Lifetime number of bytes freed:              146023272
Total number of searches:                       791866
Total number of successful allocations:         774609
Total number of failed allocations:                  0
Total number of successful frees:               772495
Total number of successful resizes:                 34
SHLL [/] #
```


```
(rshell@my_grisp_board_1)6> inet:i().
Port Module   Recv Sent Owner     Local Address      Foreign Address    State     Type
32   inet_tcp 0    0    <0.243.0> *:4369             *:*                ACCEPTING STREAM
48   inet_tcp 0    0    <0.247.0> *:60761            *:*                ACCEPTING STREAM
80   inet_tcp 4    21   <0.245.0> localhost:33584    localhost:4369     ????      STREAM
96   inet_tcp 21   4    <0.244.0> localhost:4369     localhost:33584    ????      STREAM
240  inet_tcp 8397 5569 <0.297.0> 169.254.16.1:60761 169.254.1.90:50706 ????      STREAM
432  inet_tcp 1285 2488 <0.323.0> 169.254.16.1:60761 169.254.16.2:11057 ????      STREAM
448  inet_tcp 1281 2533 <0.325.0> 169.254.16.1:60761 169.254.16.7:59240 ????      STREAM
464  inet_tcp 1285 2537 <0.327.0> 169.254.16.1:60761 169.254.16.3:35852 ????      STREAM
480  inet_tcp 1281 2537 <0.329.0> 169.254.16.1:60761 169.254.16.4:59240 ????      STREAM
560  inet_tcp 1919 2659 <0.345.0> 169.254.16.1:51030 169.254.16.5:56825 ????      STREAM
656  inet_tcp 1463 1729 <0.381.0> 169.254.16.1:64314 169.254.16.6:56825 ????      STREAM
ok
(rshell@my_grisp_board_1)7> inet:i().
Port Module   Recv  Sent Owner     Local Address      Foreign Address    State     Type
32   inet_tcp 0     0    <0.243.0> *:4369             *:*                ACCEPTING STREAM
48   inet_tcp 0     0    <0.247.0> *:60761            *:*                ACCEPTING STREAM
80   inet_tcp 4     21   <0.245.0> localhost:33584    localhost:4369     ????      STREAM
96   inet_tcp 21    4    <0.244.0> localhost:4369     localhost:33584    ????      STREAM
240  inet_tcp 10688 7628 <0.297.0> 169.254.16.1:60761 169.254.1.90:50706 ????      STREAM
432  inet_tcp 3017  4224 <0.323.0> 169.254.16.1:60761 169.254.16.2:11057 ????      STREAM
448  inet_tcp 3017  4269 <0.325.0> 169.254.16.1:60761 169.254.16.7:59240 ????      STREAM
464  inet_tcp 3017  4273 <0.327.0> 169.254.16.1:60761 169.254.16.3:35852 ????      STREAM
480  inet_tcp 3017  4273 <0.329.0> 169.254.16.1:60761 169.254.16.4:59240 ????      STREAM
560  inet_tcp 3655  4395 <0.345.0> 169.254.16.1:51030 169.254.16.5:56825 ????      STREAM
656  inet_tcp 3199  3465 <0.381.0> 169.254.16.1:64314 169.254.16.6:56825 ????      STREAM
ok
```

## Simple Erlang Node

- `1 ERLANG SHELL`
- `6 RTEMS SHELLS`
- `ERLANG DISTRIBUTION`
- `SAMPLE APPLICATION`
- `7 NODES`

```
(rshell@my_grisp_board_1)8> erlang:memory().
[{total,11141760},
 {processes,2994236},
 {processes_used,2993488},
 {system,8147524},
 {atom,292645},
 {atom_used,267856},
 {binary,18992},
 {code,4404734},
 {ets,194488}]
(rshell@my_grisp_board_1)9>
```

```
(rs@MBP)101> rp([ rpc:call(N,erlang,nodes,[]) || N <- nodes() ]).
[[rshell@my_grisp_board_2,rshell@my_grisp_board_7,
  rshell@my_grisp_board_5,rshell@my_grisp_board_3,
  rshell@my_grisp_board_4,rs@MBP,rshell@my_grisp_board_1],
 [rshell@my_grisp_board_2,rshell@my_grisp_board_6,
  rshell@my_grisp_board_5,rshell@my_grisp_board_3,
  rshell@my_grisp_board_4,rs@MBP,rshell@my_grisp_board_1],
 [rshell@my_grisp_board_2,rshell@my_grisp_board_7,
  rshell@my_grisp_board_5,rshell@my_grisp_board_6,
  rshell@my_grisp_board_4,rs@MBP,rshell@my_grisp_board_1],
 [rshell@my_grisp_board_3,rshell@my_grisp_board_6,
  rshell@my_grisp_board_7,rshell@my_grisp_board_4,rs@MBP,
  rshell@my_grisp_board_1,rshell@my_grisp_board_5],
 [rshell@my_grisp_board_6,rshell@my_grisp_board_7,
  rshell@my_grisp_board_3,rshell@my_grisp_board_4,rs@MBP,
  rshell@my_grisp_board_1,rshell@my_grisp_board_2],
 [rshell@my_grisp_board_6,rshell@my_grisp_board_7,
  rshell@my_grisp_board_5,rshell@my_grisp_board_2,
  rshell@my_grisp_board_3,rs@MBP,rshell@my_grisp_board_1],
 [rs@MBP,rshell@my_grisp_board_2,rshell@my_grisp_board_7,
  rshell@my_grisp_board_3,rshell@my_grisp_board_4,
  rshell@my_grisp_board_5,rshell@my_grisp_board_6]]
ok
(rs@MBP)102> rp([ rpc:call(N,erlang,memory,[]) || N <- nodes() ]).
[[{total,10581504},
  {processes,2437984},
  {processes_used,2437912},
  {system,8143520},
  {atom,292645},
  {atom_used,267732},
  {binary,7504},
  {code,4409799},
  {ets,193720}],
 [{total,10564144},
  {processes,2419776},
  {processes_used,2419704},
  {system,8144368},
  {atom,292645},
  {atom_used,267732},
  {binary,8488},
  {code,4409799},
  {ets,193712}],
 [{total,10564208},
  {processes,2420168},
  {processes_used,2420096},
  {system,8144040},
  {atom,292645},
  {atom_used,267732},
  {binary,8416},
  {code,4409799},
  {ets,193456}],
 [{total,10565512},
  {processes,2422072},
  {processes_used,2422000},
  {system,8143440},
  {atom,292645},
  {atom_used,267732},
  {binary,7672},
  {code,4409799},
  {ets,193760}],
 [{total,10575688},
  {processes,2432872},
  {processes_used,2432800},
  {system,8142816},
  {atom,292645},
  {atom_used,267732},
  {binary,7048},
  {code,4409799},
  {ets,193712}],
 [{total,10565360},
  {processes,2423396},
  {processes_used,2423396},
  {system,8141964},
  {atom,292645},
  {atom_used,267732},
  {binary,6552},
  {code,4409799},
  {ets,193384}],
 [{total,11131952},
  {processes,2982724},
  {processes_used,2982652},
  {system,8149228},
  {atom,292645},
  {atom_used,267856},
  {binary,20560},
  {code,4404734},
  {ets,194488}]]
ok
(rs@MBP)103>
```

Tracing and Profiling Functions The function 
`dbg:c(Mod, Fun, Args, TraceFlags)` is ideal for trace 
and profile functions executed from the shell. If the
`TraceFlags` argument is omitted, all flags are set. In the example that follows,
we trace all activity related to an `io:format/1` call, in this case,
`io:format("hello~n")`. This tracing shows the inner workings of the input/output
mechanism, under which messages are sent to the group leader. The calling
process is suspended when it goes into a receive statement, and the process is
scheduled as soon as a response from the group leader returns:

```
1> dbg:c(io, format, ["Hello World~n"]). 
Hello World 
(<0.53.0>) <0.23.0> !
{io_request,<0.53.0>,<0.23.0>, {put_chars,io_lib,format,["Hello World~n",[]]}}
(<0.53.0>) out {io,wait_io_mon_reply,2} 
(<0.53.0>) << {io_reply,<0.23.0>,ok}
(<0.53.0>) in {io,wait_io_mon_reply,2} 
(<0.53.0>) << timeout ok 
```

Using `dbg:c/3` is
ideal if you want to monitor memory usage and time spent garbage collecting in a
particular function, as it isolates the call in a single process. It is not the
best way to trace side effects using the `set_on_link` and `set_on_spawn` flags, as
all flags are cleared as soon as the function returns.



```
rebar3 grisp build --clean true --configure true
===> Verifying dependencies...
===> Checking out Erlang/OTP 22.0
* Cloning...  (this may take a while)
* Cleaning...
* Copying C code...
* Patching OTP to include sys, driver and NIF files
===> Building
* Running autoconf...
* Running configure...  (this may take a while)
* Compiling...  (this may take a while)
===> sh(./otp_build boot -a)
failed with return code 1 and the following output:
 MAKE	depend
 MAKE	generate
 GEN	x86_64-apple-darwin19.4.0/gen_git_version.mk
 GEN	x86_64-apple-darwin19.4.0/opt/smp/OPCODES-GENERATED
 GEN	x86_64-apple-darwin19.4.0/opt/smp/TABLES-GENERATED
 GEN	x86_64-apple-darwin19.4.0/opt/smp/erl_alloc_types.h
 GEN	x86_64-apple-darwin19.4.0/erl_version.h
 GEN	x86_64-apple-darwin19.4.0/opt/smp/driver_tab.c
 GEN	x86_64-apple-darwin19.4.0/opt/smp/GENERATED
 GEN	x86_64-apple-darwin19.4.0/preload.c
 MAKE	depend
 GEN	x86_64-apple-darwin19.4.0/opt/smp/depend.mk
 MAKE	depend
 GEN	obj/x86_64-apple-darwin19.4.0/opt/depend.mk
make[4]: Nothing to be done for `depend'.
 GEN	x86_64-apple-darwin19.4.0/gen_git_version.mk
 GEN	x86_64-apple-darwin19.4.0/gen_git_version.mk
make[2]: Nothing to be done for `depend'.
 MAKE	depend
make[2]: Nothing to be done for `depend'.
 MAKE	emulator
 MAKE	opt
 GEN	x86_64-apple-darwin19.4.0/gen_git_version.mk
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_main.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/preload.o
 EMU_CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_emu.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_process.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_opcodes.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_load.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_bif_load.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_debug.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_bp.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_catches.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/code_ix.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/beam_ranges.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_alloc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_mtrace.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_alloc_util.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_goodfit_alloc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bestfit_alloc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_afit_alloc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_init.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_atom_table.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_table.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_ddll.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_guard.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_info.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_op.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_os.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_lists.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_persistent.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_atomics.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_counters.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_trace.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_unique.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_wrap.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_nfunc_sched.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_guard_bifs.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_dirty_bif_wrap.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_trace.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/copy.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/utils.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/bif.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/io.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_printf_term.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_debug.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_md5.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_message.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_proc_sig_queue.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_process_dict.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_process_lock.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_port_task.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_arith.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/time.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_time_sup.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/external.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/dist.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/binary.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_db.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_db_util.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_db_hash.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_db_tree.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_thr_progress.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/big.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/hash.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/index.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/atom.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/module.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/export.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/register.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/break.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_async.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_lock_check.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_gc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_lock_count.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_posix_str.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bits.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_math.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_fun.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_port.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_term.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_node_tables.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_monitor_link.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_process_dump.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_hl_timer.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_cpu_topology.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_drv_thread.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_chksum.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_re.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_unicode.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/packet_parser.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/safe_hash.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_zlib.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_nif.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_bif_binary.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_ao_firstfit_alloc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_thr_queue.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_sched_spec_pre_alloc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_ptab.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_map.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_msacc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_lock_flags.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_io_queue.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_db_catree.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/socket_dbg.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/socket_tarray.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/socket_util.o
nifs/common/socket_util.c:987:10: warning: incompatible pointer types passing '__darwin_suseconds_t *' (aka 'int *') to parameter of type 'long *' [-Wincompatible-pointer-types]
    if (!GET_LONG(env, eUSec, &timeP->tv_usec))
         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nifs/common/socket_int.h:378:62: note: expanded from macro 'GET_LONG'
#define GET_LONG(E, TE, LP)         enif_get_long((E), (TE), (LP))
                                                             ^~~~
beam/erl_nif_api_funcs.h:104:79: note: passing argument to parameter 'ip' here
ERL_NIF_API_FUNC_DECL(int,enif_get_long,(ErlNifEnv*, ERL_NIF_TERM term, long* ip));
                                                                              ^
1 warning generated.
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_flxctr.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/sys.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/sys_drivers.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/sys_env.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/sys_uds.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/driver_tab.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/elib_memmove.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/gzio.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/unix_prim_file.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/sys_float.o
sys/unix/sys_float.c:804:1: warning: no previous prototype for function 'matherr' [-Wmissing-prototypes]
matherr(struct exception *exc)
^
sys/unix/sys_float.c:803:1: note: declare 'static' if the function is not intended to be used outside of this translation unit
int
^
static
1 warning generated.
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/sys_time.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_poll.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_check_io.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_mseg.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_mmap.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_osenv.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_unix_sys_ddll.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_mtrace_sys_wrap.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_sys_common_misc.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_os_monotonic_time_extender.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_poll.flbk.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erlang_lttng.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/inet_drv.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/ram_file_drv.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/ttsl_drv.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/erl_tracer_nif.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/prim_buffer_nif.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/prim_file_nif.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/zlib_nif.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/socket_nif.o
nifs/common/socket_nif.c:16249:11: warning: unused variable 'xres' [-Wunused-variable]
    char* xres;
          ^
1 warning generated.
 CC	obj/x86_64-apple-darwin19.4.0/opt/smp/net_nif.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_latin_1_table.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_compile.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_config.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_dfa_exec.o
 GEN	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/pcre_exec_loop_break_cases.inc
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_exec.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_fullinfo.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_get.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_globals.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_maketables.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_newline.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_ord2utf8.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_refcount.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_study.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_tables.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_valid_utf8.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_version.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_byte_order.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_jit_compile.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_string_utils.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_ucd.o
 CC	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/pcre_xclass.o
 AR	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/libepcre.a
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: /Users/laymer/Dev/rshell/_grisp/otp/22.0/build/erts/emulator/pcre/obj/x86_64-apple-darwin19.4.0/opt/libepcre.a(pcre_string_utils.o) has no symbols
 MAKE	opt
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/ethr_aux.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/ethr_atomics.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/ethr_mutex.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/ethr_cbf.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/ethread.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/ethr_event.o
 AR	../lib/internal/x86_64-apple-darwin19.4.0/libethread.a
 RANLIB	../lib/internal/x86_64-apple-darwin19.4.0/libethread.a
 CC	obj/x86_64-apple-darwin19.4.0/opt/erl_memory_trace_parser.o
 AR	../lib/x86_64-apple-darwin19.4.0/liberts.a
 RANLIB	../lib/x86_64-apple-darwin19.4.0/liberts.a
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/erl_memory_trace_parser.o
 AR	../lib/x86_64-apple-darwin19.4.0/liberts_r.a
 RANLIB	../lib/x86_64-apple-darwin19.4.0/liberts_r.a
 CC	obj/x86_64-apple-darwin19.4.0/opt/erl_printf_format.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/erl_printf.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/erl_misc_utils.o
 AR	../lib/internal/x86_64-apple-darwin19.4.0/liberts_internal.a
 RANLIB	../lib/internal/x86_64-apple-darwin19.4.0/liberts_internal.a
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/erl_printf_format.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/erl_printf.o
 CC	obj/x86_64-apple-darwin19.4.0/opt/r/erl_misc_utils.o
 AR	../lib/internal/x86_64-apple-darwin19.4.0/liberts_internal_r.a
 RANLIB	../lib/internal/x86_64-apple-darwin19.4.0/liberts_internal_r.a
 GEN	obj/x86_64-apple-darwin19.4.0/opt/MADE
 LD	/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/bin/x86_64-apple-darwin19.4.0/beam.smp
ld: weak import of symbol '___darwin_check_fd_set_overflow' not supported because of option: -no_weak_imports for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make[4]: *** [/Users/laymer/Dev/rshell/_grisp/otp/22.0/build/bin/x86_64-apple-darwin19.4.0/beam.smp] Error 1
make[3]: *** [opt] Error 2
make[2]: *** [opt] Error 2
make[1]: *** [smp] Error 2
make: *** [emulator] Error 2

 ✘ laymer@MBP  ~/Dev/rshell 
```

## Memory Footprint reports

- The same data has a weight of 344 Kb if the option is not specified
- 3000 concurrent ETS writes of Pmod sensor data weigh 284 Kb when ETS table is declared with compression instruction

- 30000 concurrent ETS writes of Pmod sensor data weigh 3094 Kb when ETS table is declared with compression instruction


- 300000 concurrent ETS writes of Pmod sensor data weigh 11602 Kb when ETS table is declared with compression instruction
- 300000 concurrent ETS writes of Pmod sensor data weigh 38156 Kb when ETS table is 
NOT declared with compression instruction

__NOTE : for the sake of studying limitations, a test with a 1M updates from each process i.e.
3M of sensor data entries in a non compressed ETS table resulted in over 3 million objects
stored in the table, occupying 382 MB of space. Meanwhile, the compressed variant weighs 281 MB
for the same amount of entries (3M). In resource constrained environments, this can be a major
factor of succes, and failure if ignored. For example the 100 MB delta between the 2 experiments
corresponds roughly to the volume available on 2 completely empty GRiSP embedded systems put together. Assuming that 382 MB could theoretically fit into 6 completely empty boards, using the built-in compression feature from ETS, that number could immediately decrease to a total of 4.
In other words, 33% less space would be required.__


# Lasp Cluster 

## `eheap_alloc` crash with 7 nodes

- `7 RTEMS SHELLS`
- `PARTISAN DISTRIBUTION`
- `ACHLYS APPLICATION WITH SMALL TASKS`
- `7 NODES`
- `DEFAULT PEER SERVICE MANAGER (NO HYPARVIEW, FULL-MESH)`

```
grisp.ini: section "network", name "ip_self", value "169.254.16.1"
=== Ip is 169.254.16.1 ===
grisp.ini: section "network", name "wlan_ip_netmask", value "255.255.0.0"
grisp.ini: section "network", name "wlan_mode", value "adhoc"
grisp.ini: section "network", name "wlan_adhocname", value "edge"
grisp.ini: section "network", name "wlan_channel", value "6"
grisp.ini: section "network", name "hostname", value "my_grisp_board_1"
erl.rtems -- -mode embedded -noshell -noinput -home . -pa . -root achlys -config achlys/releases/0.4.1/sys.config -boot achlys/releases/0.4.1/achlys -internal_epmd epmd_sup -kernel inetrc "./erl_inetrc" -sname achlys -setcookie MyCookie
uhub0: 2 ports with 2 removable, self powered
ugen0.2: <Philips Semiconductors ISP1520> at usbus0
uhub1: <Philips Semiconductors ISP1520, class 9/0, rev 2.00/0.00, addr 2> on usbus0
uhub1: 3 ports with 3 removable, self powered
ugen0.3: <Realtek 802.11n NIC> at usbus0
rtwn0 on uhub1
rtwn0: <Realtek 802.11n NIC, class 0/0, rev 2.00/0.00, addr 3> on usbus0
rtwn0: MAC/BB RTL8188EU, RF 6052 1T1R
wlan0: Ethernet address: 38:1d:d9:46:2d:e6
mkdir /tmp
mkdir /tmp/log
mkdir /home
Setting environment
chdir(/media/mmcsd-0-0/)

erl_main: starting ...
getcwd: /media/mmcsd-0-0
hostname: my_grisp_board_1
starting erlang runtime
 =========================
 Starting RTEMS shell...
 =========================

RTEMS Shell on /dev/console. Use 'help' to list commands.
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             150
Largest free block:                           26593760
Total bytes free:                             30824472
Number of used blocks:                            2091
Largest used block:                            8388616
Total bytes used:                             29706208
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              30824296
Maximum number of free blocks ever:                153
Maximum number of blocks searched ever:            125
Lifetime number of bytes allocated:           31028096
Lifetime number of bytes freed:                1325368
Total number of searches:                        21692
Total number of successful allocations:           7262
Total number of failed allocations:                  0
Total number of successful frees:                 5171
Total number of successful resizes:                 18
SHLL [/] # 00:04:36.883 [info] Using node name: achlys@my_grisp_board_1
00:04:56.300 [info] Partisan listening on {169,254,16,1}:27000 listen_addrs: [#{ip => {169,254,16,1},port => 27000}]
00:04:56.323 [info] Not using container orchestration; disabling.
00:04:56.351 [info] node achlys@my_grisp_board_1 choosing random seed: {91909116,275527329464,-134217711}
00:04:56.444 [info] node achlys@my_grisp_board_1 choosing random seed: {91909116,275527329464,-134217711}
00:04:56.652 [info] Setting jitter: false
00:04:57.110 [info] Setting jitter percent: 1
00:04:57.549 [info] Setting event interval: 0
00:04:57.981 [info] Setting max events: 1000
00:04:58.405 [info] Setting extended logging: false
00:04:58.880 [info] Setting mailbox logging: false
00:04:59.326 [info] Setting operation mode: state_based
00:04:59.756 [info] Setting set type: orset
00:05:00.639 [info] Setting broadcast: false
00:05:08.184 [info] Membership: false
00:05:08.624 [info] Workflow: false
00:05:10.352 [info] AdClientEnabled: false
00:05:11.226 [info] AdServerEnabled: false
00:05:11.663 [info] TournClientEnabled: false
00:05:12.106 [info] TournServerEnabled: false
00:05:12.532 [info] ThroughputType: gset
00:05:13.010 [info] ThroughputClientEnabled: false
00:05:13.453 [info] ThroughputServerEnabled: false
00:05:13.903 [info] DivergenceType: gcounter
00:05:14.342 [info] DivergenceClientEnabled: false
00:05:14.777 [info] DivergenceServerEnabled: false
00:05:15.327 [info] Backend initialized with pid: <0.881.0>
00:05:15.338 [info] Backend lasp_ets_storage_backend initialized: <0.881.0>

SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             168
Largest free block:                            2883576
Total bytes free:                             17191128
Number of used blocks:                            2118
Largest used block:                            8388616
Total bytes used:                             43339552
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              16143536
Maximum number of free blocks ever:                169
Maximum number of blocks searched ever:            125
Lifetime number of bytes allocated:          172748904
Lifetime number of bytes freed:              129468712
Total number of searches:                        45213
Total number of successful allocations:          26930
Total number of failed allocations:                  0
Total number of successful frees:                24812
Total number of successful resizes:                 73
SHLL [/] # 00:06:50.549 [error] CRASH REPORT Process <0.995.0> with 0 neighbours crashed with reason: {register_mismatch,acc,who_am_i,<<0>>} in pmod_nav:verify_reg/2 line 281
00:13:43.344 [error] gen_server partisan_pluggable_peer_service_manager terminated with reason: no function clause matching lists:foldl(#Fun<partisan_util.1.377106>, {dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => ...,...},...],...],...}}}, {badrpc,nodedown}) line 1262
00:13:45.968 [error] CRASH REPORT Process partisan_pluggable_peer_service_manager with 10 neighbours crashed with reason: no function clause matching lists:foldl(#Fun<partisan_util.1.377106>, {dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => ...,...},...],...],...}}}, {badrpc,nodedown}) line 1262
00:13:55.077 [error] gen_server <0.810.0> terminated with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:13:55.518 [error] Supervisor partisan_sup had child partisan_pluggable_peer_service_manager started with partisan_pluggable_peer_service_manager:start_link() at <0.798.0> exit with reason no function clause matching lists:foldl(#Fun<partisan_util.1.377106>, {dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => ...,...},...],...],...}}}, {badrpc,nodedown}) line 1262 in context child_terminated
00:13:57.855 [error] gen_server <0.807.0> terminated with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:13:58.319 [info] node achlys@my_grisp_board_1 choosing random seed: {91909116,275527329464,-134217711}
00:13:58.508 [error] gen_server <0.808.0> terminated with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:13:59.416 [error] gen_server <0.809.0> terminated with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:13:59.972 [error] gen_server lasp_state_based_synchronization_backend terminated with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:14:00.455 [error] CRASH REPORT Process <0.810.0> with 0 neighbours exited with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:14:00.719 [error] CRASH REPORT Process <0.807.0> with 0 neighbours exited with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:14:01.101 [error] CRASH REPORT Process <0.808.0> with 0 neighbours exited with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:14:01.321 [error] CRASH REPORT Process <0.809.0> with 0 neighbours exited with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:14:03.466 [error] Supervisor partisan_pool had child {partisan_peer_service_server,{{169,254,16,4},19003},
                              {{169,254,16,1},27000},
                              #Ref<0.3241694241.4154982401.94099>} started with {partisan_peer_service_server,acceptor_init,undefined} at <0.810.0> exit with reason {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in context child_terminated
00:14:04.302 [error] Supervisor partisan_pool had child {partisan_peer_service_server,{{169,254,16,2},61388},
                              {{169,254,16,1},27000},
                              #Ref<0.3241694241.4154982401.94096>} started with {partisan_peer_service_server,acceptor_init,undefined} at <0.807.0> exit with reason {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in context child_terminated
00:14:05.176 [error] Supervisor partisan_pool had child {partisan_peer_service_server,{{169,254,16,5},30354},
                              {{169,254,16,1},27000},
                              #Ref<0.3241694241.4154982401.94097>} started with {partisan_peer_service_server,acceptor_init,undefined} at <0.808.0> exit with reason {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in context child_terminated
00:14:06.065 [error] Supervisor partisan_pool had child {partisan_peer_service_server,{{169,254,16,3},29173},
                              {{169,254,16,1},27000},
                              #Ref<0.3241694241.4154982401.94098>} started with {partisan_peer_service_server,acceptor_init,undefined} at <0.809.0> exit with reason {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in context child_terminated
00:14:06.352 [error] CRASH REPORT Process lasp_state_based_synchronization_backend with 3 neighbours exited with reason: {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in gen_server:call/3 line 223
00:14:08.486 [error] Supervisor lasp_sup had child lasp_distribution_backend started with lasp_distribution_backend:start_link() at <0.878.0> exit with reason {{function_clause,[{lists,foldl,[#Fun<partisan_util.1.377106>,{dict,10,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[[#{listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2},{#{ip => {169,254,16,2},port => 27000},undefined,<0.1377.0>}],[#{listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3},{#{ip => {169,254,16,3},port => 27000},undefined,<0.1379.0>}],[#{channels => [undefined],listen_addrs => ...,...},...]],...}}},...],...},...]},...} in context child_terminated
00:14:09.636 [info] Backend initialized with pid: <0.1420.0>
00:14:09.661 [info] Backend lasp_ets_storage_backend initialized: <0.1420.0>
00:15:34.673 [warning] global: achlys@my_grisp_board_1 failed to connect to achlys@my_grisp_board_6
00:15:35.369 [warning] global: achlys@my_grisp_board_1 failed to connect to achlys@my_grisp_board_7
00:16:00.689 [info] Connection cache miss for node: achlys@my_grisp_board_7
00:17:39.401 [info] Connection cache miss for node: achlys@my_grisp_board_7
00:17:49.999 [info] Connection cache miss for node: achlys@my_grisp_board_7
00:18:00.729 [info] Connection cache miss for node: achlys@my_grisp_board_7
00:18:13.615 [info] Connection cache miss for node: achlys@my_grisp_board_7
00:18:24.092 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{membership_strategy,{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{full_v1,<<246,72,70,187,43,12,89,99,249,231,179,10,182,224,178,47,138,158,138,17>>,{state_orset,[{#{channels => [undefined],listen_addrs => [#{ip => {169,254,1,90},port => 27000}],name => achlys@MBP,parallelism => 1},[{<<253,73,88,1,45,126,67,134,188,160,215,44,203,197,154,209,182,200,199,92>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},[{<<86,230,172,153,152,111,74,19,141,140,199,87,38,224,83,221,69,130,55,133>>,true},{<<105,117,238,242,39,81,30,153,7,178,125,145,120,27,82,180,34,143,168,241>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2,parallelism => 1},[{<<121,116,8,109,106,249,1,63,105,37,101,171,6,2,112,87,110,31,112,176>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3,parallelism => 1},[{<<244,224,8,33,104,46,236,43,255,143,140,107,208,143,24,31,123,225,246,59>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name => achlys@my_grisp_board_4,parallelism => 1},[{<<191,228,192,5,217,54,90,231,160,247,173,106,43,255,132,111,29,197,234,74>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,5},port => 27000}],name => achlys@my_grisp_board_5,parallelism => 1},[{<<173,221,166,118,66,168,123,28,231,50,88,249,76,46,148,231,154,114,240,125>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},[{<<177,238,156,227,34,163,236,234,141,2,4,71,39,164,137,235,41,37,149,95>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,7},port => 27000}],name => achlys@my_grisp_board_7,parallelism => 1},[{<<10,249,191,87,173,207,90,185,84,23,207,211,180,252,168,38,22,49,98,185>>,true}]}]}}}}} failed to send: {error,closed}
00:18:24.142 [info] Message {forward_message,lasp_state_based_synchronization_backend,{'$gen_cast',{state_send,achlys@my_grisp_board_1,{{<<"tasks">>,state_gset},state_gset,[{clock,[{<<0,95,23,151,206,41,34,56,33,123,183,165,22,240,211,49,37,39,49,28>>,100}]}],{state_gset,[]}},false}}} failed to send: {error,closed}
00:18:24.169 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{ping,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},{567,994681,365523}}} failed to send: {error,closed}
00:18:24.408 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{membership_strategy,{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{full_v1,<<246,72,70,187,43,12,89,99,249,231,179,10,182,224,178,47,138,158,138,17>>,{state_orset,[{#{channels => [undefined],listen_addrs => [#{ip => {169,254,1,90},port => 27000}],name => achlys@MBP,parallelism => 1},[{<<253,73,88,1,45,126,67,134,188,160,215,44,203,197,154,209,182,200,199,92>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},[{<<86,230,172,153,152,111,74,19,141,140,199,87,38,224,83,221,69,130,55,133>>,true},{<<105,117,238,242,39,81,30,153,7,178,125,145,120,27,82,180,34,143,168,241>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2,parallelism => 1},[{<<121,116,8,109,106,249,1,63,105,37,101,171,6,2,112,87,110,31,112,176>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3,parallelism => 1},[{<<244,224,8,33,104,46,236,43,255,143,140,107,208,143,24,31,123,225,246,59>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name => achlys@my_grisp_board_4,parallelism => 1},[{<<191,228,192,5,217,54,90,231,160,247,173,106,43,255,132,111,29,197,234,74>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,5},port => 27000}],name => achlys@my_grisp_board_5,parallelism => 1},[{<<173,221,166,118,66,168,123,28,231,50,88,249,76,46,148,231,154,114,240,125>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},[{<<177,238,156,227,34,163,236,234,141,2,4,71,39,164,137,235,41,37,149,95>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,7},port => 27000}],name => achlys@my_grisp_board_7,parallelism => 1},[{<<10,249,191,87,173,207,90,185,84,23,207,211,180,252,168,38,22,49,98,185>>,true}]}]}}}}} failed to send: {error,closed}
00:18:24.465 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994776,109751}}} failed to send: {error,closed}
00:18:24.495 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994786,321554}}} failed to send: {error,closed}
00:18:24.540 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994796,455612}}} failed to send: {error,closed}
00:18:24.576 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994806,588923}}} failed to send: {error,closed}
00:18:24.607 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994817,225511}}} failed to send: {error,closed}
00:18:24.664 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994827,550965}}} failed to send: {error,closed}
00:18:24.698 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994837,698713}}} failed to send: {error,closed}
00:18:24.736 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994847,794565}}} failed to send: {error,closed}
00:18:24.780 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994860,931616}}} failed to send: {error,closed}
00:18:24.807 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994871,53342}}} failed to send: {error,closed}
00:18:24.844 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994881,814505}}} failed to send: {error,closed}
00:18:24.875 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{ping,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},{567,994693,849618}}} failed to send: {error,closed}
00:18:24.905 [info] Message {forward_message,lasp_state_based_synchronization_backend,{'$gen_cast',{state_send,achlys@my_grisp_board_1,{{<<"tasks">>,state_gset},state_gset,[{clock,[{<<0,95,23,151,206,41,34,56,33,123,183,165,22,240,211,49,37,39,49,28>>,115}]}],{state_gset,[]}},false}}} failed to send: {error,closed}
00:18:25.113 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{membership_strategy,{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{full_v1,<<246,72,70,187,43,12,89,99,249,231,179,10,182,224,178,47,138,158,138,17>>,{state_orset,[{#{channels => [undefined],listen_addrs => [#{ip => {169,254,1,90},port => 27000}],name => achlys@MBP,parallelism => 1},[{<<253,73,88,1,45,126,67,134,188,160,215,44,203,197,154,209,182,200,199,92>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},[{<<86,230,172,153,152,111,74,19,141,140,199,87,38,224,83,221,69,130,55,133>>,true},{<<105,117,238,242,39,81,30,153,7,178,125,145,120,27,82,180,34,143,168,241>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,2},port => 27000}],name => achlys@my_grisp_board_2,parallelism => 1},[{<<121,116,8,109,106,249,1,63,105,37,101,171,6,2,112,87,110,31,112,176>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,3},port => 27000}],name => achlys@my_grisp_board_3,parallelism => 1},[{<<244,224,8,33,104,46,236,43,255,143,140,107,208,143,24,31,123,225,246,59>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,4},port => 27000}],name => achlys@my_grisp_board_4,parallelism => 1},[{<<191,228,192,5,217,54,90,231,160,247,173,106,43,255,132,111,29,197,234,74>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,5},port => 27000}],name => achlys@my_grisp_board_5,parallelism => 1},[{<<173,221,166,118,66,168,123,28,231,50,88,249,76,46,148,231,154,114,240,125>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},[{<<177,238,156,227,34,163,236,234,141,2,4,71,39,164,137,235,41,37,149,95>>,true}]},{#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,7},port => 27000}],name => achlys@my_grisp_board_7,parallelism => 1},[{<<10,249,191,87,173,207,90,185,84,23,207,211,180,252,168,38,22,49,98,185>>,true}]}]}}}}} failed to send: {error,closed}
00:18:25.210 [info] Message {forward_message,partisan_pluggable_peer_service_manager,{pong,#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 27000}],name => achlys@my_grisp_board_6,parallelism => 1},#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,1},port => 27000}],name => achlys@my_grisp_board_1,parallelism => 1},{567,994892,178594}}} failed to send: {error,closed}
00:18:25.786 [info] Connection cache miss for node: achlys@my_grisp_board_7


eheap_alloc: Cannot allocate 2059352 bytes of memory (of type "heap").

Crash dump is being written to: erl_crash.dump...
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             189
Largest free block:                            1572856
Total bytes free:                             12994288
Number of used blocks:                            2149
Largest used block:                            8388616
Total bytes used:                             47536392
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:              10897488
Maximum number of free blocks ever:                195
Maximum number of blocks searched ever:            189
Lifetime number of bytes allocated:          205722832
Lifetime number of bytes freed:              157197224
Total number of searches:                        77021
Total number of successful allocations:          52138
Total number of failed allocations:                  4 <---------------
Total number of successful frees:                49989
Total number of successful resizes:                 77
SHLL [/] # done
```

### Analysis
 
```
eheap_alloc: Cannot allocate 2 059 352 bytes of memory (of type "heap").
---------------------------------------------------------------------
Number of free blocks:                             189
Largest free block:                          1 572 856
Total bytes free:                           12 994 288
Number of used blocks:                           2 149
Largest used block:                          8 388 616
Total bytes used:                           47 536 392
```

```
Largest free block:                          1 572 856 (1)
eheap allocation attempt Size                2 059 352 (2)
----------------------------------------------------------
											   486 496
```

The required block size is 486 496 bytes larger than
the largest available block in memory. __But 12 994 288
bytes in 189 blocks i.e. 68.752 bytes/block on average
are still free. Therefore the Erlang VM crash could have
been avoided if free space was contiguous, or if the
eheap_alloc allocations were divided into smaller blocks.__

### Reproducing on another node

#### Remote Erlang Shell Side

```
(achlys@my_grisp_board_1)27> 

Als = fun() -> [ lasp:update({<<"light">>,state_gset}
	, {add, {node(), erlang:monotonic_time(), pmod_als:percentage()}}
	, self()) 	
		|| X <- lists:seq(1,100000) ] end.

#Fun<erl_eval.21.91303403>

(achlys@my_grisp_board_1)28> spawn(fun() -> Als() end).

<0.14939.0>

(achlys@my_grisp_board_1)29> erlang:memory().                                                                                       
[{total,23566088},
 {processes,7589684},
 {processes_used,7587560},
 {system,15976404},
 {atom,650749},
 {atom_used,637286},
 {binary,172376},
 {code,9640450},
 {ets,475912}]

(achlys@my_grisp_board_1)30> erlang:memory().
[{total,22627896},
 {processes,6686824},
 {processes_used,6684988},
 {system,15941072},
 {atom,650749},
 {atom_used,637286},
 {binary,138736},
 {code,9640450},
 {ets,474560}]
(achlys@my_grisp_board_1)31>
```

#### RTEMS Shell

```
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             192
Largest free block:                            1572856
Total bytes free:                             11431776
Number of used blocks:                            2141
Largest used block:                            8388616
Total bytes used:                             49098904
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:               9334456
Maximum number of free blocks ever:                195
Maximum number of blocks searched ever:            192
Lifetime number of bytes allocated:          289351144
Lifetime number of bytes freed:              239263024
Total number of searches:                       211816
Total number of successful allocations:         174766
Total number of failed allocations:                  4 <---------------
Total number of successful frees:               172625
Total number of successful resizes:                 75
SHLL [/] # 00:59:31.715 [info] Connection cache miss for node: achlys@my_grisp_board_2
SHLL [/] # malloc
C Program Heap and RTEMS Workspace are the same.
Number of free blocks:                             192
Largest free block:                            1572856
Total bytes free:                             11431776
Number of used blocks:                            2141
Largest used block:                            8388616
Total bytes used:                             49098904
Size of the allocatable area in bytes:        60530680
Minimum free size ever in bytes:               9334456
Maximum number of free blocks ever:                195
Maximum number of blocks searched ever:            192
Lifetime number of bytes allocated:          289420728
Lifetime number of bytes freed:              239332608
Total number of searches:                       212270
Total number of successful allocations:         175220
Total number of failed allocations:                  4 <---------------
Total number of successful frees:               173079
Total number of successful resizes:                 75
SHLL [/] # eheap_alloc: Cannot allocate 3332104 bytes of memory (of type "old_heap").

Crash dump is being written to: erl_crash.dump...done
al extension: source=5, is_internal=0, error=1
```

__NOTE : eheap_alloc is still causing the crash,
but the allocation type is "old_heap"
in this case, while in the first example it was "heap".__ 

On average, a cluster of 7 nodes uses up to 20 MB of
additional memory with Achlys, Lasp, Partisan and subsequent
dependencies. The Erlang VM shows a total footprint of around 23 MB
while RTEMS reports that nearly 50 MB are in use. 

With the sample application and Erlang native distribution, the VM
was standing at a consistent 11 MB total, and RTEMS reported 31 MB used.
Therefore, 2 issues can be observed :

 - 1. `eheap_alloc` attempts to allocate
 	chunks of memory that are much larger
 	than the actual largest free block 
 	available in memory.
 - 2. The memory footprint reported by
 	the Erlang VM is inaccurate w.r.t. the
 	actual memory usage. The 10 MB increase
 	in total compared to the sample cluster
 	shown by `erlang:memory()` does not match
 	the almost twice bigger increase reported
 	by RTEMS. 

#### Crash cause
The crashes happen due to the reason explained in point N°1
above. In fact, after the Erlang VM has crashed, there is
still unused memory that could have been sufficient if contiguous.
The problem is that very large blocks of continuous free space are
asked by `eheap_alloc`, and as soon as the allocation attempts fail,
the VM crashes. 

#### Possible solutions

 - Different allocation strategies
 - Reducing eheap_alloc block sizes
 - Defragmentation

