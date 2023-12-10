;;; mood-line-test.el --- Test specifications for mood-line.el -*- lexical-binding: t; -*-

(add-to-list 'load-path ".")

(require 'ert)
(require 'mood-line)

;;; Code:

;; -------------------------------------------------------------------------- ;;
;;
;; Macros
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; mood-line-defformat
;; ---------------------------------- ;;

(ert-deftest -defformat/padding ()
  "The expanded sequence should include the provided (or default) padding."
  (should (equal (mood-line-defformat)
                 (list
                  ;; Left
                  '(" ")
                  ;; Right
                  '(" "))))
  (should (equal (mood-line-defformat
                  :padding
                  "---")
                 (list
                  ;; Left
                  '("---")
                  ;; Right
                  '("---")))))

(ert-deftest -defformat/left-right-nil ()
  "The format sequence should expand if the left or right segment list is nil."
  (should (equal (mood-line-defformat
                  :left
                  ("XYZ"))
                 (list
                  ;; Left
                  '(" " "XYZ")
                  ;; Right
                  '(" "))))
  (should (equal (mood-line-defformat
                  :right
                  ("XYZ"))
                 (list
                  ;; Left
                  '(" ")
                  ;; Right
                  '("XYZ" " ")))))

(ert-deftest -defformat/left-right ()
  "The expanded sequence should include left and right segments lists."
  (should (equal (mood-line-defformat
                  :left
                  ("ABC")
                  :right
                  ("XYZ"))
                 (list
                  ;; Left
                  '(" " "ABC")
                  ;; Right
                  '("XYZ" " ")))))

(ert-deftest -defformat/cons-cells ()
  "Cons cell segments should expand into their `car' and `cdr' values."
  (should (equal (mood-line-defformat
                  :left
                  ("ABC" ("ABC" . "XYZ") "XYZ")
                  :right
                  ("..." ((some-fn) . " ") "..."))
                 (list
                  ;; Left
                  '(" " "ABC" "ABC" "XYZ" "XYZ")
                  ;; Right
                  '("..." (some-fn) " " "..." " ")))))

(ert-deftest -defformat/exp-separators ()
  "Non-string, non-cons expressions should expand followed by a blank string."
  (should (equal (mood-line-defformat
                  :left
                  ("ABC" ("ABC" . "XYZ") some-exp "XYZ" (some-fn))
                  :right
                  ("..." ((some-fn) . " ") (another-fn) "..."))
                 (list
                  ;; Left
                  '(" " "ABC" "ABC" "XYZ" some-exp "" "XYZ" (some-fn) "")
                  ;; Right
                  '("..." (some-fn) " " (another-fn) "" "..." " ")))))

;; -------------------------------------------------------------------------- ;;
;;
;; Helper functions
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; mood-line--get-glyph
;; ---------------------------------- ;;

(ert-deftest --get-glyph/unicode ()
  "Glyphs should be fethed from `mood-line-glyph-alist'."
  (let ((mood-line-glyph-alist '((:checker-info . ?🛈))))
    (should (string= (mood-line--get-glyph :checker-info) "🛈"))))

(ert-deftest --get-glyph/fallback-ascii ()
  "Glyphs should be fetched from `mood-line-glyphs-ascii' as a fallback."
  (let ((mood-line-glyph-alist nil))
    (should (string= (mood-line--get-glyph :checker-info) "i"))))

;; ---------------------------------- ;;
;; mood-line--process-segments
;; ---------------------------------- ;;

(ert-deftest --process-segments/default ()
  "`mood-line-format-default' should be processed without error."
  (let* ((left (car mood-line-format-default))
         (right (cadr mood-line-format-default))
         (left-str (mood-line--process-segments left))
         (right-str (mood-line--process-segments right)))
    (should (> (length left-str) 0))
    (should (> (length right-str) 0))))

(ert-deftest --process-segments/strings ()
  "Literal strings should be concatenated."
  (let* ((segments '("ABC" "   " "123" "   " "XYZ"))
         (segments-str (mood-line--process-segments segments)))
    (should (string= segments-str "ABC   123   XYZ"))))

(ert-deftest --process-segments/nil-neighbor-exclusion ()
  "A nil value should mean skipping evaluation of the following segment."
  (let* ((segments '("ABC" nil "   " "123" nil "   " "XYZ" nil))
         (segments-str (mood-line--process-segments segments)))
    (should (string= segments-str "ABC123XYZ"))))

(ert-deftest --process-segments/functions ()
  "Functions should be evaluated, and their return values concatenated."
  (let* ((segments '((string ?A ?B ?C)
                     (numberp nil) "   "
                     (number-to-string (+ 100 20 3))
                     (identity nil) "   "
                     (concat "X" "Y" "Z")))
         (segments-str (mood-line--process-segments segments)))
    (should (string= segments-str "ABC123XYZ"))))

;; ---------------------------------- ;;
;; mood-line--process-format
;; ---------------------------------- ;;

(ert-deftest --process-format/default ()
  "`mood-line-format-default' should be processed without error."
  (let* ((format-str (mood-line--process-format mood-line-format-default)))
    (should (> (length format-str) 0))))

(ert-deftest --process-format/nil-okay ()
  "Either segment list should process without error if the other list is nil."
  (let* ((format-l-nil '(nil ("ABC" nil "   " "123" nil "   " "XYZ")))
         (format-r-nil '(("ABC" nil "   " "123" nil "   " "XYZ") nil))
         (format-l-nil-str (mood-line--process-format format-l-nil))
         (format-r-nil-str (mood-line--process-format format-r-nil)))
    (should (string-suffix-p "ABC123XYZ" format-l-nil-str))
    (should (string-prefix-p "ABC123XYZ" format-r-nil-str))))

;; -------------------------------------------------------------------------- ;;
;;
;; mood-line-mode
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; mood-line--activate
;; ---------------------------------- ;;

(ert-deftest --activate/mode-line-format ()
  "`mode-line-format' should be set by `mood-line--activate'."
  (let* ((setting-val (alist-get 'mode-line-format
                                 mood-line--settings-alist))
         (mood-line--settings-alist `((mode-line-format . ,setting-val)))
         (mood-line-format mood-line-format-default)
         (mode-line-format '("ABC 123 XYZ"))
         (format-str (mood-line--process-format mood-line-format)))
    (mood-line--activate)
    (should (string= (format-mode-line mode-line-format)
                     (format-mode-line format-str)))))

;; ---------------------------------- ;;
;; mood-line--deactivate
;; ---------------------------------- ;;

(ert-deftest --deactivate/mode-line-format ()
  "`mode-line-format' should be restored by `mood-line--deactivate'."
  (let* ((setting-val (alist-get 'mode-line-format
                                 mood-line--settings-alist))
         (mood-line--settings-alist `((mode-line-format . ,setting-val)))
         (mood-line--settings-backup-alist nil)
         (mood-line-format mood-line-format-default)
         (mode-line-format "ABC 123 XYZ"))
    (mood-line--activate)
    (mood-line--deactivate)
    (should (string= mode-line-format "ABC 123 XYZ"))))

;;; mood-line-test.el ends here
