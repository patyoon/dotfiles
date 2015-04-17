;;; Package --- Summary
;;; Commentary:
;;; Java mode setup.


(setq my-el-get-java-packages
      (append
       '(
         ;; https://github.com/senny/emacs-eclim
         eclim
         ))
      )

(el-get 'sync my-el-get-java-packages)

(add-hook 'java-mode-hook 'gradle-mode)
(setq eclim-auto-save t)
(global-eclim-mode)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(custom-set-variables
 '(eclim-eclipse-dirs '("~/nonStandard/eclipse"))
 '(eclim-executable "~/nonStandard/eclipse/eclim"))

(provide `java)
;;; java.el ends here.
