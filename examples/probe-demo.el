;; ./probe-demo.sh

(let ((base (file-name-directory (or buffer-file-name load-file-name))))
  (setq ga-base-dir (file-name-directory (substring base 0 -1))))

(add-to-list 'load-path (concat ga-base-dir "src"))
(add-to-list 'load-path (concat ga-base-dir "tests"))

(require 'cl)
(require 'gv)
(require 'rkt)
(require 'ga144-sim)
(require 'ga-loadup)
(ga-loadup)

(setq code "
node 705
: x 0x3ffff !b 0x3fff0 !b ;
: s . . . . ;
: main
io b!
10 for
0x2b !b 0x2a !b
s
0x3a !b 0x3b !b
x x x x x x
s s s s
next
warm ;
")

(setq assembled (assemble (aforth-compile code)))

(setq chip (ga144-new "host"))
(setq node705 (send chip coord->node 705))

(ga-connect-probe node705 0)
(ga-connect-probe node705 1)
(ga-connect-probe node705 2)
(ga-connect-probe node705 3)

(send chip load assembled)

(ga144-step*)

(ga144-probe-save)

(when (file-exists-p "ga144-probe-graph.py")
  (shell-command "python ga144-probe-graph.py"))
