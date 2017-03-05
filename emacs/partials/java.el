;;; Package --- Summary
;;; Commentary:
;;; Java mode setup.

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; Override style from original java style.
(c-add-style "my-java-style"
             '("java"
               (c-indent-level . 2)
               (c-basic-offset . 2)
               (c-hanging-braces-alist
                ((substatement-open)
                 (block-close . c-snug-do-while)
                 (extern-lang-open after)
                 (inexpr-class-open after)
                 (inexpr-class-close before)))
               (c-offsets-alist
                (substatement-open . 0))
               ))

(defun my-java-mode-hook nil
  (setq c-default-style "my-java-style")
  (c-set-style "my-java-style")
  (setq whitespace-line-column 100)
  )

(add-hook 'java-mode-hook 'my-java-mode-hook)

(provide `java)
;;; java.el ends here.
