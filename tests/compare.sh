#!/bin/bash

#compare racket and elisp compilation results

../dump $@ > /tmp/out_1
../aforth $@ > /tmp/out_2
diff /tmp/out_1 /tmp/out_2

