#!/usr/bin/env python
from sys import argv
from serial import Serial
import struct


def listen(serial):
    def read_n( n ):
        x = [ord(serial.read( 1 )) for _ in range( n ) ]
        x.reverse()
        word = 0
        for byte in x:
            word = ( word << 8 ) | byte
            n -= 1
        return word

    while True:
        n = read_n( 1 )
        if n  == 1:
            print "[exit]"
            return
        if n == 0:
            n = read_n( 3 )
            print n & 0x3ffff
        else:
            print "unknown code:", s

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
    serial.reset_input_buffer()
    listen(serial)
    serial.close()
