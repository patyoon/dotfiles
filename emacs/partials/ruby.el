;; Causing weird indentation in continuation line.
;;
;; Resources
;; http://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx/ ;; http://compgroups.net/comp.emacs/ruby-mode-indentation-of-continuation-lines/375117
;; TODO (patrick): Implement better indentation method that does
;; def foo(bar,
;;         baz)
;; Also better to have newline-and-indent method binding.
(setq ruby-deep-indent-paren nil)

(setq my-el-get-ruby-packages
      (append
       '(
          ;;robe-mode
          enh-ruby-mode
          rails-el
          ;; Needed to run erubis with rvm for erb syntax checking.
          ;; https://github.com/senny/rvm.el
          rvm
          ;;rspec-mode
          ))
      )

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(el-get 'sync my-el-get-ruby-packages)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(rvm-use-default)

(require 'highlight-indentation)
(add-hook 'ruby-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))

(defun my-ruby-mode-hook nil
  (setq whitespace-line-column 100))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(setq ruby-indent-level 2)

(provide `ruby)
;;; ruby.el ends here
