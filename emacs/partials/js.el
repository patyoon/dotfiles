;;; Package --- Summary
;;; Commentary:
;;; JavaScript mode setup.
;;; Referred https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/

;; Use js2-mode.
(add-to-list 'auto-mode-alist '("\\.\\(?:js\\|jsx\\|json\\)\\'" . js2-mode))

(setq js-indent-level 2)

(setq js2-basic-offset 2)

(setq my-el-get-js-packages
      (append
       '(
         js2-mode
         ))
      )

(el-get 'sync my-el-get-js-packages)

(add-hook 'js-mode-hook 'js2-minor-mode)

;; jshint with flycheck.
;; https://truongtx.me/2014/02/21/emacs-setup-jshint-for-on-the-fly-petential-error-checking/
;; Need to run npm install -g jshint first.
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; jsx syntax highlight setup.
;; https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/
;; Need to run npm install -g jsxhint first.
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(provide `js)
;;; js.el ends here.
