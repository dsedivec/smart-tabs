;; Based on code from <http://www.emacswiki.org/emacs/SmartTabs>.

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

;;;###autoload
(defmacro smart-tabs-advice (function &rest offset-vars)
  `(progn
     ,@(mapcar (lambda (var) `(defvaralias ',var 'tab-width)) offset-vars)
     (defadvice ,function (around smart-tabs activate)
       (cond
         ((and smart-tabs-mode indent-tabs-mode)
          (save-excursion
            (beginning-of-line)
            (while (looking-at "\t*\\( +\\)\t+")
              (replace-match "" nil nil nil 1)))
          (let ((tab-width fill-column))
            ad-do-it))
         (t
          ad-do-it)))))

(provide 'smart-tabs)
