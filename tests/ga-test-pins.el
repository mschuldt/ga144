;; tests pin callbacks and breakpoints

(define-test-fn "test-pins"
  (lambda ()
    (setq host (ga144-new "host"))

    (setq code "node  705
: aa 0 !b ;
: bb ;
: main
io b!
0x3ffff !b
aa bb
warm
")

    (setq compiled (aforth-compile code))
    (setq assembled (assemble compiled))

    (setq node (ga144-get-node host 705))

    (setq pin0-config nil)
    (setq pin1-config nil)
    (setq pin2-config nil)
    (setq pin3-config nil)

    (send node set-gpio-handlers
          (lambda (x) (message "pin0: %s" x)
            (setq pin0-config x))
          (lambda (x) (setq pin1-config x))
          (lambda (x) (setq pin2-config x))
          (lambda (x) (setq pin3-config x)))

    ;;code must be loaded before named breakpoints can be set
    (send host load assembled)
    (send node set-breakpoint "aa")
    (send node set-breakpoint "bb")

    (ga144-step*)
    ;;(send node describe-io)
    (setq ok (check 3 3 3 3))
    (ga144-step*)
    ;;(send node describe-io)
    (and (check 0 0 0 0) ok)
    ))

(defun printer (pin)
  (lambda (val) (printf "pin ~a: ~a\n" pin val)))

(defun report (pin expect actual)
  (message "pin %s has value %s, expected %s" pin actual expect))

(defun check (a b c d)
  (let ((ok t))
    (unless (eq a pin0-config)
      (report 0 a pin0-config)
      (setq ok nil))
    (unless (eq b pin1-config)
      (report 1 b pin1-config)
      (setq ok nil))
    (unless (eq c pin2-config)
      (report 2 c pin2-config)
      (setq ok nil))
    (unless (eq d pin3-config)
      (report 3 d pin3-config)
      (setq ok nil))
    ok))

(provide 'ga-test-pins)
