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

(provide `java)
;;; java.el ends here.
