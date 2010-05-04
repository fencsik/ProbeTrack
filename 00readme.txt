00readme.txt: desciption of contents of ProbeTrack project

Miscellaneous directories:

* Code: directory for all current experiment code (other directories
  are outdated, and code should be moved here if it's updated)
* conf: conference materials
* stpaper: Single-task ProbeTrack paper

Experiments:

* probetrack01: basic probetrack experiment--track 4 out of 8 with SOAs of 0,
  80, 160, 320, and 1280 ms--with a 133 ms gap
* probetrack02: added more SOAs to probetrack01--0, 40, 80, 120, 160, and 1280
  ms
* probetrack03: added multiple gap durations--133, 307, and 507 ms--and more
  SOAs--0, 40, 80, 120, 160, 320, 960, and 1280 ms--to probetrack01
* probetrack04: probetrack01 with 307 ms gaps, SOAs of 0, 80, 320, and 1280 ms,
  tracking 1, 2, 3, or 4 targets out of 8 disks
* probetrack05: control for probetrack04 without gaps to estimate baseline
  performance at each level of tracking load
* probetrack06: replicates probetrack04 with more trials per cell and SOAs of
  0, 40, 80, and 160 ms (didn't work; need longer SOA)
* probetrack06b: replicates probetrack04 with more trials per cell and SOAs of
  0, 40, 80, and 1280 ms
* probetrack07: replicates probetrack06b with only set sizes 3 and 4; run by
  CSUEB Psych 3100 in 2008-4
* probetrack08: replicates probetrack07 with tracking loads 2 and 4; run as
  pilot study to prep for dual-task experiments
* probetrack09: replicates probetrack07 with tracking load 2 only
* probetrack10: ProbeTrack with 3 targets and different, generally shorter
  probe delays, and an attempt to get reliable effects with 20 trials/cell.
