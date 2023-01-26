;;; mood-line-segment-modal.el --- A modal editing segment for mood-line -*- lexical-binding: t; -*-
;;
;; Author: trevDev() <trev@trevdev.ca>
;;         Jessie Hildebrandt <jessieh.net>
;; Homepage: https://gitlab.com/jessieh/mood-line

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This segment displays modal editing information for the current buffer.

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

;; -------------------------------------------------------------------------- ;;
;;
;; Custom definitions
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Variable definitions
;; ---------------------------------- ;;

(defcustom mood-line-evil-state-alist
  '((normal . ("<N>" . font-lock-variable-name-face))
    (insert . ("<I>" . font-lock-string-face))
    (visual . ("<V>" . font-lock-keyword-face))
    (replace . ("<R>" . font-lock-type-face))
    (motion . ("<M>" . font-lock-constant-face))
    (operator . ("<O>" . font-lock-function-name-face))
    (emacs . ("<E>" . font-lock-builtin-face)))
  "Set the string and corresponding face for any `evil-mode' state.
The `Face' may be either a face symbol or a property list of key-value pairs
 e.g. (:foreground \"red\")."
  :group 'mood-line
  :type '(alist
          :key-type symbol
          :value-type
          (cons (string :tag "Display Text") (choice :tag "Face" face plist))))

(defcustom mood-line-meow-state-alist
  '((normal . ("<N>" . font-lock-variable-name-face))
    (insert . ("<I>" . font-lock-string-face))
    (keypad . ("<K>" . font-lock-keyword-face))
    (beacon . ("<B>" . font-lock-type-face))
    (motion . ("<M>" . font-lock-constant-face)))
  "Set the string and corresponding face for any `meow-mode' state.
The `Face' may be either a face symbol or a property list of key-value pairs
 e.g. (:foreground \"red\")."
  :group 'mood-line
  :type '(alist
          :key-type symbol
          :value-type
          (cons (string :tag "Display Text") (choice :tag "Face" face plist))))

;; -------------------------------------------------------------------------- ;;
;;
;; Modal editing segment
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Evil segment function
;; ---------------------------------- ;;

(defun mood-line-segment-modal--evil ()
  "Display the current evil-mode state."
  (when (boundp 'evil-state)
    (let ((mode-cons (alist-get evil-state mood-line-evil-state-alist)))
      (concat (propertize (car mode-cons)
                          'face (cdr mode-cons))
              " "))))

;; ---------------------------------- ;;
;; Meow segment function
;; ---------------------------------- ;;

(defun mood-line-segment-modal--meow ()
  "Display the current meow-mode state."
  (when (boundp 'meow--current-state)
    (let ((mode-cons (alist-get
                      meow--current-state
                      mood-line-meow-state-alist)))
      (concat (propertize (car mode-cons)
                          'face (cdr mode-cons))
              " "))))

;; ---------------------------------- ;;
;; God segment function
;; ---------------------------------- ;;

(defun mood-line-segment-modal--god ()
  "Indicate whether or not god-mode is active."
  (if (bound-and-true-p god-local-mode)
      '(:propertize "<G> "
                    face (:inherit mood-line-status-warning))
    "--- "))

;; -------------------------------------------------------------------------- ;;
;;
;; Provide package
;;
;; -------------------------------------------------------------------------- ;;

(provide 'mood-line-segment-modal)

;;; mood-line-segment-modal.el ends here
