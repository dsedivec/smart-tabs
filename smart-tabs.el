;; Based on code from <http://www.emacswiki.org/emacs/SmartTabs>.

;;;###autoload
(define-minor-mode smart-tabs-mode
    "Toggle \"smart tabs\" mode."
  :lighter " SmTab")

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
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
         ((and smart-tabs-mode indent-tabs-mode)
          (save-excursion
            (beginning-of-line)
            (while (looking-at "\t*\\( +\\)\t+")
              (replace-match "" nil nil nil 1)))
          (setq tab-width tab-width)
          (let ((tab-width fill-column)
                (,offset fill-column)
                (wstart (window-start)))
            (unwind-protect
                 (progn ad-do-it)
              (set-window-start (selected-window) wstart))))
         (t
          ad-do-it)))))

(provide 'smart-tabs)
