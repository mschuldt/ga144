#!/usr/bin/env python

from sys import argv
import serial

#18bit words transmitted using 4 bytes with format:
#     upper2 middle8 lower8 0xa5
#
# use 0x00947 to terminate
#     0x2222  to read input
def listen(port, speed):
    ser = serial.Serial(port, speed)
    t0 = time.time()
    while True:
        s = ser.read(4)
        (v, ) = struct.unpack("<I", s)
        if (v & 0xff) == 0xa5:
            v >>= 8
            if 33 <= v < 127:
                printable = "'%c'" % v
            else:
                printable = ""
            print "[%.3f]" % (time.time() - t0),
            print "0x%05x  %d   %s" % (v & 0x3ffff, v & 0x3ffff, printable)
            if (v & 0xffff) == 0x1111: # ???
                t0 = time.time()
            if (v & 0xffff) == 0x2222:
                # print 'took', time.time() - t0
                print 'respond'
                ser.write(self.sget([1]))
                # ser.flush()
            if v == 0x00947:
                print "Terminated."
                exit(1);
            t0 = time.time()

if __name__ == "__main__":
    if len(argv != 3):
        print('Usage: ')
        print('./listen.py port speed')
    port = argv[1]
    speed = int(argv[2])

    listen(port, speed)
