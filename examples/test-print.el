
;; functions used in the test-print.aforth and test-print2.aforth examples

(ga-define showT
           (princ (format "%s\n" (car (send node get-dstack-as-list)))))

(setq __s nil)

(ga-define saveT
           (push (car (send node get-dstack-as-list)) __s)
           )

(ga-define end
           (princ (format "%s\n" __s))
           )


