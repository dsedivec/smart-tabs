;;; smart-tabs.el --- indent with tabs, align with spaces  -*- lexical-binding: t; -*-
;; Copyright (C) 2017  Dale Sedivec
;;
;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: tools indenting
;; Version: 3
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This started with the code posted at
;; <http://www.emacswiki.org/emacs/SmartTabs> and has evolved from
;; there.
;;
;; I *think* this differs from
;; <https://github.com/jcsalomon/smarttabs/> in that it allows you to
;; specify more than one offset variable when applying it to a mode.
;;
;;; Code:

(require 'cl-lib)

;;;###autoload
(define-minor-mode smart-tabs-mode
    "Toggle \"smart tabs\" mode."
  :lighter " SmTab"
  ;; Make sure this has a buffer local value, because we'll be
  ;; changing it and we don't want other buffers to see our huge
  ;; outrageous value during indentation processing, as unlikely as
  ;; that is.
  (make-local-variable 'tab-width))

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode (if smart-tabs-mode nil indent-tabs-mode)))
    ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode (if smart-tabs-mode nil indent-tabs-mode)))
    ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode (if smart-tabs-mode nil indent-tabs-mode)))
    ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode (if (and smart-tabs-mode
                                   (memq indent-line-function
                                         '(indent-relative
                                           indent-relative-maybe)))
                              nil
                            indent-tabs-mode)))
    ad-do-it))

(defvar smart-tabs-advised-functions nil
  "Alist of advised functions to variables modified by the advice.")

;;;###autoload
(defun smart-tabs-advise (function &rest offset-vars)
  "Advise indentation FUNCTION to enable \"smart tabs\".

OFFSET-VARS is the list of variables that will be temporarily
updated to achieve the smart tabs effect."
  (let* ((existing-offset-vars (alist-get function
                                          smart-tabs-advised-functions))
         (all-offset-vars (cl-nunion existing-offset-vars offset-vars)))
    (setf (alist-get function smart-tabs-advised-functions) all-offset-vars)
    (advice-add function :around
                (eval `(lambda (orig-fun &rest args)
                         (cond
                           ((and smart-tabs-mode indent-tabs-mode)
                            (save-excursion
                              (beginning-of-line)
                              (while (looking-at "\t*\\( +\\)\t+")
                                (replace-match "" nil nil nil 1)))
                            (let* ((tab-width fill-column)
                                   ,@(mapcar (lambda (var) `(,var tab-width))
                                             all-offset-vars))
                              (apply orig-fun args)))
                           (t
                            (apply orig-fun args)))))
                '((name . smart-tabs)))))

(provide 'smart-tabs)
;;; smart-tabs.el ends here
