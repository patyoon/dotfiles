;;; Package --- Summary
;;; Commentary:
;;; Ruby mode setup.

;; (defadvice ruby-indent-line (after unindent-closing-paren activate)
;;   (let ((column (current-column))
;;         indent offset)
;;     (save-excursion
;;       (back-to-indentation)
;;       (let ((state (syntax-ppss)))
;;         (setq offset (- column (current-column)))
;;         (when (and (eq (char-after) ?\))
;;                    (not (zerop (car state))))
;;           (goto-char (cadr state))
;;           (setq indent (current-indentation)))))
;;     (when indent
;;       (indent-line-to indent)
;;       (when (> offset 0) (forward-char offset)))))

;; Causing weird indentation in continuation line.
;;
;; Resources
;; http://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx/
;; http://compgroups.net/comp.emacs/ruby-mode-indentation-of-continuation-lines/375117
;; TODO (patrick): Implement better indentation method that does
;; def foo(bar,
;;         baz)
;; Also better to have newline-and-indent method binding.
(setq ruby-deep-indent-paren nil)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

(require 'auto-complete-config)
(add-to-list 'ac-modes 'ruby-mode)

;; (defun my-ruby-mode-hook ()
;;   "custom hooks for ruby mode."
;;   (setq whitespace-line-column 100)
;;   (add-hook 'ruby-mode-hook 'rubocop-mode)
;;   )
;; (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; Indentation of Ruby statements.
(setq ruby-indent-level 2)
;; To avoid indentation like:
;; default_attributes({
;;                    'instance_metadata' => {
;;                    'security_groups' => [
;;                                          'allow_nerd_ssh',
(setq ruby-use-smie nil)
(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-bounce-deep-indent t)

;; (defadvice ruby-indent-line (after unindent-closing-paren activate)
;;   "Indent sole parenthesis in loca's way."
;;   (let ((column (current-column))
;;         indent offset)
;;     (save-excursion
;;       (back-to-indentation)
;;       (let ((state (syntax-ppss)))
;;         (setq offset (- column (current-column)))
;;         (when (and (eq (char-after) ?\))
;;                    (not (zerop (car state))))
;;           (goto-char (cadr state))
;;           (setq indent (current-indentation)))))
;;     (when indent
;;       (indent-line-to indent)
;;       (when (> offset 0) (forward-char offset)))))

(add-hook 'ruby-mode-hook (lambda () (setq-local indent-line-function 'ruby-indent-line)))

(provide `ruby)
;;; ruby.el ends here
