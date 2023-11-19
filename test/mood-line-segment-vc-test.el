;;; mood-line-segment-vc-test.el --- Test specifications for mood-line-segment-vc.el -*- lexical-binding: t; -*-

(add-to-list 'load-path ".")

(require 'ert)
(require 'mood-line)
(require 'mood-line-segment-vc)

;;; Code:

;; -------------------------------------------------------------------------- ;;
;;
;; Helper functions
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; mood-line-segment-vc--rev
;; ---------------------------------- ;;

(ert-deftest --rev/git ()
  "Current rev should be cleanly extracted from git `vc-mode' status string."
  (should (string= (mood-line-segment-vc--rev " Git:main" 'Git)
                   "main")))

(ert-deftest --rev/hg ()
  "Current rev should be cleanly extracted from hg `vc-mode' status string."
  (should (string= (mood-line-segment-vc--rev " Hg:main" 'Hg)
                   "main")))

(ert-deftest --rev/unknown ()
  "Current rev should be reported as \"???\" when backend is unsupported."
  (let ((buffer-file-name ""))
    (should (string= (mood-line-segment-vc--rev "" 'SVN)
                     "???"))))

;;; mood-line-segment-vc-test.el ends here
