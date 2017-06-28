
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Serial-Ports.html

(defun ga144-scan ()
  "scan for connected GA144 devices"
  (error "ga144-scan unimplemented"))

(defun ga144-find-serial-port ()
  (error "ga144-find-serial-port unimplemented")
  )

(defun ga-get-connected-serial-devices ()
  (directory-files "/dev/serial/by-id" t ".*\\([^.]\\|[^..]\\)$"))

;;(ga-make-serial "/dev/ttyS0" 9600)

(defun ga144-serial-open (port speed &optional flowcontrol sentinel filter)
  (when (eq port 'auto)
    (setq port (ga144-find-serial-port)))
  (make-serial-process :port port
                       :speed speed
                       ;;:flowcontrol (or flowcontrol 'hw)
                       :sentinel (or sentinel 'ga-serial-sentinel)
                       :filter (or filter 'ga-serial-filter)
                       :coding 'no-conversion
                       :noquery t
                       ))

(defun ga-serial-close (serial)
  "close SERIAL port"
  (delete-process serial))

(defun ga-serial-sentinel (proc state)
  "Sentinel for GA serial port processes"
  (message "ga-serial-sentinel %s -- '%s'" proc state))

(defun ga-serial-filter (proc str)
  "Filter for GA serial port processes"  
  (message "received input form process %s" proc)
  (message "  input = %s" str)
  (message "  words = %s" (ga-process-input proc str))
  )

(defun ga-build-word (byte-array index)
  (logand (logior (ash (aref byte-array index) 16)
                  (ash (aref byte-array (+ index 1) 8))
                  (aref byte-array (+ index 2)))
          #x3ffff)  
  )

(defun ga-process-input (serial str)
  "Process input byte string from GA144 SERIAL processs,
returns list of transmitted words"
  (message "ga-process-input %s %s" serial str)
  (let ((bytes (string-to-vector str))
        (n-bytes (length bytes))
        (n-words (/ n-bytes 4))
        (alive t)
        code words word byte)
    (when (not (= (* n-words 4) n-bytes))
      (message "ERROR: received partial input TODO FIX"))
    
    (dotimes (and alive (i n-words))
      (setq code (aref bytes (+ i 3)))
      (cond ((= code 0) ;;read word
             (push (ga-build-word bytes i) words))
            ((= code 1) ;;exit
             (setq alive nil)
             (ga-serial-close serial)
             (message "*** ga144 serial closed by device ***"))
            ((= code 2) ;;input
             (ga-serial-request-input serial (ga-build-word bytes i)))
            (t (message "Received invalid word code: %s" code))))
    words))

(defun ga-serial-request-input (serial n-bytes)
  "read N-BYTES from user and send to SERIAL process"
  )

(defun ga-serial-write (serial bytes)
  (process-send-string serial (mapconcat 'char-to-string bytes "")))

(defun ga-serial-read (port)
  )

(provide 'ga-serial)
