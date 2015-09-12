#!/usr/bin/env python

from cal import *
from serial import Serial

speed = 460800
if len(argv) not in [2,3]:
    print "Usage: ./dump-rom.py port [speed={}]".format(speed)
    exit(1)

port = argv[1]

bootstream = run_command("racket rom-dump-bootstream.rkt")
serial = Serial(port, speed)
write("".join(map(chr, bootstream)), serial)
listen(port, speed, serial, verbose=False)
