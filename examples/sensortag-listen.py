#!/usr/bin/env python
from sys import argv
from serial import Serial
import struct

def read_seq():
    print ""
    for i, name in enumerate(["acc X", "acc Y", "acc Z",
                              "mag X", "mag Y", "mag Z",
                              "gyro X", "gyro Y", "gyro Z",
                              "therm",
                              "misc", "???", "???"]):
        s = serial.read(4)
        (v, ) = struct.unpack("<I", s)
        if (v & 0xff) == 0:
            val = (v >> 8) & 0xffff
            val2 = None
            if i < 3:
                if val >= 0x8000:
                    val2 = float(0x10000 - val) / 0x20000 * 10 / 1.27 * 9.8
                else:
                    val2 = float(val) / 0x20000 * 10 * -1 / 1.27 * 9.8
                #x = val & 0xfff
                #if x & 0x800:
                #    x = ((v - 0x800) + 1)*(-1)
            print name, val, val2 or ""
        else:
            print "?????????"

def listen(serial):
    while True:
        s = serial.read(4)
        (v, ) = struct.unpack("<I", s)
        if (v & 0xff) == 0:
            val = (v >> 8) & 0x3ffff
            if val == 0x3ffff:
                read_seq()
            else:
                print "(unkown)",val
        elif (v & 0xff) == 1:
            print "[exit]"
            exit(0)
        else:
            print "unknown code:", v, v & 0xff

if __name__ == "__main__":
    speed = 460800
    if len(argv) not in [2,3]:
        print "usage: ./listen.py port [speed={}]".format(speed)
        exit(1)
    if len(argv) == 3:
        speed = argv[2]
    port = argv[1]
    print "Listening. port={}, speed={}".format(port, speed)
    serial = Serial(port, speed)
    listen(serial)
    
