;; (require 'python-pep8)
;; (require 'python-pylint)

(setq python-indent-offset 2)

;; use Ropemacs
;; turn off until I figure out how to use it.
;; (add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

;; ;; ;(add-hook 'python-mode-hook 'my-python-hook)

;; ;; ;; (defun py-outline-level ()
;; ;; ;;   "This is so that `current-column` DTRT in otherwise-hidden text"
;; ;; ;;   ;; from ada-mode.el
;; ;; ;;   (let (buffer-invisibility-spec)
;; ;; ;;     (save-excursion
;; ;; ;;       (skip-chars-forward "\t ")
;; ;; ;;       (current-column))))

;; ;; ;; ; this fragment originally came from the web somewhere, but the outline-regexp
;; ;; ;; ; was horribly broken and is broken in all instances of this code floating
;; ;; ;; ; around.  Finally fixed by Charl P. Botha <http://cpbotha.net/>
;; ;; ;; (defun my-python-hook ()
;; ;; ;;   (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)")
;; ;; ;;   ; enable our level computation
;; ;; ;;   (setq outline-level 'py-outline-level)
;; ;; ;;   ; do not use their \C-c@ prefix, too hard to type. Note this overides
;; ;; ;;   ;some python mode bindings
;; ;; ;;   (setq outline-minor-mode-prefix "\C-c")
;; ;; ;;   ; turn on outline mode
;; ;; ;;   (outline-minor-mode t)
;; ;; ;;   ; initially hide all but the headers
;; ;; ;;   (hide-body)
;; ;; ;;   (show-paren-mode 1)
;; ;; ;; )

;; ;; ;(setq python-check-command "/Users/jeshua/python-setup/python-check.sh")

;; ;; ;; (add-hook 'python-mode-hook
;; ;; ;;           '(lambda ()
;; ;; ;;              (progn (define-key python-mode-map "\C-m" 'newline-and-indent))))
;; ;; ;; (add-hook 'python-mode-hook
;; ;; ;;           (function (lambda ()
;; ;; ;;                       (setq indent-tabs-mode nil
;; ;; ;;                             tab-width 2))))

;; python jedi setup
;; TODO - Add a way to install jedi server after checking if not installed.
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

;; ipython notebook
;;(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default python-indent 2)
(setq-default py-indent-offset 2)
(setq py-indent  2)
(setq py-indent-offset  2)

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
(define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)


;; python mode tab width
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

                                        ; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
                                        ; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
                                        ; don't split windows
(setq py-split-windows-on-execute-p nil)
(setq py-force-py-shell-name-p t)
;;(require 'ipython)
(setq py-smart-indentation t)

;; change comint keys for ipython shell
(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
(setq-default explicit-shell-file-name "/bin/zsh")
;; need to fix ^A things.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; let's set the python path correctly as well
;; (setenv "PYTHONPATH" (concat python-files-dir
;;                              (concat path-separator
;;                                      (getenv "PYTHONPATH"))))

;; ;;pdb setup, note the python version
;; (setq pdb-path '/usr/lib/python2.6/pdb.py
;;       gud-pdb-command-name (symbol-name pdb-path)
;;       (defadvice pdb (before gud-query-cmdline activate)
;;         "Provide a better default command line when called interactively."))
;; (interactive
;;  (list (gud-query-cmdline pdb-path
;;                           (file-name-nondirectory buffer-file-name))))

;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook #'(lambda ()
;;                                 (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
;; (add-hook 'ipython-shell-hook #'(lambda ()
;;                                   (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))

;; ;; If you want to use anything-show-completion.el,(facultative)
;; ;; <http://www.emacswiki.org/cgi-bin/emacs/anything-show-completion.el>
;; ;; add these lines:

;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                                 '(length initial-pattern)))

;;(unload-feature 'ipython)

;; (after 'auto-complete-config
;;        (ac-config-default)
;;        (when (file-exists-p (expand-file-name "/Users/patrick/.emacs.d/el-get/pymacs"))
;;          (ac-ropemacs-initialize)
;;          (ac-ropemacs-setup)))

;; (after 'auto-complete-autoloads
;;        (autoload 'auto-complete-mode "auto-complete" "enable auto-complete-mode" t nil)
;;        (add-hook 'python-mode-hook
;;                  (lambda ()
;;                    (require 'auto-complete-config)
;;                    (add-to-list 'ac-sources 'ac-source-ropemacs)
;;                    (auto-complete-mode))))


(setq virtual-env (getenv "VIRTUAL_ENV"))

;; (if (not (equal virtual-env 'nil))
;;     (setq load-path (append
;;                      (list (concat virtual-env "/src/pymacs" ))
;;                      load-path))
;;   (let ((foo 'bar))
;;     (require 'pymacs)
;;     (pymacs-load "ropemacs" "rope-")
;;     (setq ropemacs-enable-autoimport 't)
;;     ))

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

(provide 'python)
;;; python.el ends here
