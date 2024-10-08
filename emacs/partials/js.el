;;; Package --- Summary
;;; Commentary:
;;; JavaScript mode setup.
;;; Referred https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
;; TODO: Warn if `sudo npm install -g jshint` is already ran.

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

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-attr-indent-offset 2)
)

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(provide `js)
;;; js.el ends here
