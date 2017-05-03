#!/bin/bash

#compare racket and elisp compilation results

./dump --bootstream async $@ > /tmp/out_1
./ga $@ > /tmp/out_2
diff /tmp/out_1 /tmp/out_2

