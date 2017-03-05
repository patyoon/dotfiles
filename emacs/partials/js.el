;;; Package --- Summary
;;; Commentary:
;;; JavaScript mode setup.
;;; Referred https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
;; TODO: Warn if `sudo npm install -g jshint` is already ran,

;; Use js2-mode for javascript and json.
(add-to-list 'auto-mode-alist '("\\.\\(?:js\\|son\\)\\'" . js2-mode))

(setq js-indent-level 2)

(add-hook 'js-mode-hook 'js2-minor-mode)

;; jshint with flycheck.
;; https://truongtx.me/2014/02/21/emacs-setup-jshint-for-on-the-fly-petential-error-checking/
;; Need to run npm install -g jshint first.
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; jsx syntax highlight setup.
;; https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/
;; Need to run npm install -g jsxhint first.
;; Use web-mode for jsx.
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; make sure emacs exec path finds eslint.
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(setq flycheck-javascript-eslint-executable "eslint")
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq jsx-indent-level 2)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(add-hook 'jsx-mode-hook
          (lambda () (auto-complete-mode 1)))

(provide `js)

;;; js.el ends here.
