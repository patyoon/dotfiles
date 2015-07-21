;;; Package --- Summary
;;; Commentary:
;;; JavaScript mode setup.
;;; Referred https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/

(setq-default js-indent-level 2)

(setq-default js2-basic-offset 2)

(setq-default my-el-get-js-packages
      (append
       '(
         js2-mode
         ))
      )

(el-get 'sync my-el-get-js-packages)

(add-hook 'js-mode-hook 'js2-minor-mode)
;;(add-hook 'js2-mode-hook 'ac-js2-mode)

;; jshint with flycheck.
;; https://truongtx.me/2014/02/21/emacs-setup-jshint-for-on-the-fly-petential-error-checking/
;; Need to run npm install -g jshint first.
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(provide `js)
;;; js.el ends here.
