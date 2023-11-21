```lisp
;; Emacs 29 with native compilation, default GC threshold
;; Run in a lisp-interaction-mode buffer with Flymake

(defun time-mode-line (num &optional and-mem)
  (let ((gc-cons-threshold (if and-mem gc-cons-threshold most-positive-fixnum))
        (start-time (current-time)))
    (cl-loop for i to num
             do (format-mode-line mode-line-format))
    (format-time-string "%s.%3N" (time-since start-time))))

;; Default mode line:
(time-mode-line 10000)          ;; "0.440"
(time-mode-line 10000 :and-mem) ;; "2.402"

;; mood-line (default settings):
(mood-line-mode t)
(time-mode-line 10000)          ;; "0.309"
(time-mode-line 10000 :and-mem) ;; "1.286"
```
