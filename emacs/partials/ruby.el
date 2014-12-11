;; Causing weird indentation in continuation line.
;; http://compgroups.net/comp.emacs/ruby-mode-indentation-of-continuation-lines/375117
;; TODO (patrick): Implement better indentation method that does
;; def foo(bar,
;;         baz)
;; Also better to have newline-and-indent method binding.
(setq ruby-deep-indent-paren nil)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))

(feafe)

(provide `ruby)
;;; ruby.el ends here
