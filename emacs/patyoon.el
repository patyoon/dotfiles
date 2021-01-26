;;; Code: My emacs setup
;;; use el-get for package manager

;; ;; use el-get for manaing packages.
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (let (el-get-master-branch)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp))))

;;More package archives
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ;; list all packages that I want
;; (setq my-el-get-packages
;;       (append
;;        '(yasnippet
;;          zenburn
;;          yas-jit
;;          backup-each-save
;;          virtualenvwrapper
;;          auto-complete
;;          haskell-latex
;;          jinja2-mode
;;          package
;;          popup
;;          rope
;;          ropemacs
;;          pymacs
;;          pos-tip
;;          popup-pos-tip
;;          dired+
;;          jedi
;;          dired-details
;;          find-dired+
;;          company-mode
;;          ;; highlight-indentations
;;          ;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;;          highlight-indentation
;;          ;; http://emacsrocks.com/e13.html
;;          multiple-cursors
;;          )
;;        (mapcar 'el-get-source-name el-get-sources)))


;; (el-get 'sync my-el-get-packages)

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of consing.
;; By increasing it to 10 MiB we reduce the number of pauses due to garbage collection.
;; From: http://www.wilfred.me.uk/.emacs.d/init.html#orgheadline52
(setq gc-cons-threshold (* 10 1024 1024))

;; Configure to confirm before closing emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Disable Prelude whitespace mode showing whitespaces as dots.
;; (setq prelude-whitespace nil)

;; Use arrow navigation for now
(defun disable-guru-mode ()
  (guru-mode -1)
  )
(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; ;; ido-ubiquitous mode.
;; ;; First turn off icomplete to use ido instead.
;; (icomplete-mode 99)
;; ;; ido-style completion for (almost) every function that uses the standard
;; ;; completion function "completing-read".
;; ;; https://github.com/technomancy/ido-ubiquitous
;; (require 'ido)
;; (ido-mode 1)
;; (ido-ubiquitous 1)
;; '(ido-enable-last-directory-history nil)
;; '(ido-enable-regexp nil)
;; '(ido-max-directory-size 300000)
;; '(ido-max-file-prompt-width 0.1)
;; '(ido-use-filename-at-point (quote guess))
;; '(ido-use-url-at-point t)

;; M-x enhancement built on top of ido.
;; http://www.emacswiki.org/emacs/Smex
(require 'smex)
(setq smex-history-length 100)
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(global-set-key (kbd "M-x") 'smex)

;; recentf is a minor mode that builds a list of recently opened files.

;; start recentf
;; http://www.emacswiki.org/emacs/RecentFiles

;; run auto-cleanup of recentf without displaying on minibuffer.
;; https://gist.github.com/masutaka/1325654
(require 'cl)

(defvar my-recentf-list-prev nil)

(defun my-recentf-save-list ()
  "If recentf-list and previous recentf-list is equal,
do nothing"
  (unless (equal recentf-list my-recentf-list-prev)
    (recentf-save-list)
    (setq my-recentf-list-prev recentf-list)))

(defadvice write-region
  (around recentf-no-message)
  (ad-set-arg 4 'nomsg)
  ad-do-it
  (set-buffer-modified-p nil))

(defadvice recentf-save-list
  (around no-message activate)
  "suppress the output from message() and write-region() to
minibuffer"
  (let ((activated (ad-is-active 'write-region)))
    (ad-enable-advice 'write-region 'around 'recentf-no-message)
    (ad-activate 'write-region)
    (unwind-protect
	(flet ((message (format-string &rest args)
			(eval `(format ,format-string ,@args))))
	  ad-do-it)
      (ad-disable-advice 'write-region 'around 'recentf-no-message)
      (if activated
	  (ad-activate 'write-region)
	(ad-deactivate 'write-region)))))

(defadvice recentf-cleanup
  (around no-message activate)
  "suppress the output from message() to minibuffer"
  (flet ((message (format-string &rest args)
		  (eval `(format ,format-string ,@args))))
    ad-do-it))

(add-to-list 'recentf-exclude "\\.ido\\.last")
(add-to-list 'recentf-exclude "recentf")
(global-set-key (kbd "\C-x f") 'prelude-recentf-ido-find-file)
;; C-x b recent file finding. This feature relies upon the recentf package.
(setq ido-use-virtual-buffers 1)
(setq recentf-auto-cleanup 10)
;;end recentf

;; Turn on linum mode to display line number on the right.
(global-linum-mode t)
(setq linum-format "%d ")

;; for fixing broken fringe-mode issue.
(set-fringe-mode '(0 . 0))

;;Make lines wrap instead of going over edge
(global-visual-line-mode)

;; Delete extra whitespace when killing lines.
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
   Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; indentation setting.

;; start indent-mode.
(setq-default indent-line-function 'indent-relative)
;; ‘tab-stop-list’ holds a list of all the tab stops to use, when ‘indent-relative’ does not find an
;; appropriate tab stop.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60))))

;; Set default indentation to 2.
(setq default-tab-width 2)

;;  http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
;; Permanently force Emacs to indent with spaces, never with TABs:
(setq-default indent-tabs-mode nil)
;; end indent.

;;  provides the function for displaying a tooltip at mouse
;;  position which allows users to easily show it.
;;(require 'pos-tip)
;;(require 'popup-pos-tip)
;; (defadvice popup-tip
;; (around popup-pos-tip-wrapper (string &rest args) activate)
;; (if (eq window-system 'x)
;;              (apply 'popup-pos-tip string args)
;;          ad-do-it))

;; Autocomplete mode: nicer autocomplete rendering
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(global-auto-complete-mode t)
(ac-config-default)
;;Needed for compatibility with flyspell
(ac-flyspell-workaround)
(setq ac-auto-start 3)
(setq ac-auto-show-menu 0.5)
;;(setq ac-quick-help-delay .5)
(setq ac-use-quick-help -1)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
;; Make \C-n and \C-p work in autocompletion menu
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-ignore-case nil)

;;Disable flymake GUI warnings (they cause crash)
(setq flymake-gui-warnings-enabled nil)

;; Scroll to end of window before error
(setq scroll-error-top-bottom t)

;; Stop creating those backup~ files
(setq make-backup-files nil)
(setq backup-by-copying t)

(global-subword-mode 1) ;move thru camelCaseWords

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
(autoload 'haskell-latex-mode "")

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
;; (add-to-list 'el-get-recipe-path (expand-file-name "~/prg/el-get/recipes"))
(require 'package)
(package-initialize)

(load-theme 'misterioso t)
(setq custom-safe-themes t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq prelude-theme 'misterioso)

(setq TeX-PDF-mode t)
;; set alt keys for meta
(setq mac-option-modifier 'meta)

;;no popup when opening buffer from terminal
(setq-default ns-pop-up-frames nil)

(defun remove-trailing-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))
(add-hook 'write-file-hooks 'remove-trailing-whitespace)

;; yas with ido-ubiquotous
;;  Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

;; (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; tramp to use server nicknames
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

;; (toggle-debug-on-error 1)
;; Disable smart parens mode

;;  pos-tip
;; (require 'popup-pos-tip)
;; (defadvice popup-tip
;;   (around popup-pos-tip-wrapper (string &rest args) activate)
;;   (if (eq window-system 'x)
;;       (apply 'popup-pos-tip string args)
;;     ad-do-it))

;; (set-variable temporary-file-directory "/tmp")
;;    (defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
;;       (setq backup-directory-alist
;;           `((".*" . ,emacs-tmp-dir)))
;;       (setq auto-save-file-name-transforms
;;           `((".*" ,emacs-tmp-dir t)))
;;       (setq auto-save-list-file-prefix
;;           emacs-tmp-dir)
;;   (setq auto-save-file-name-transforms
;;            '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
;;             ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "/usr/local/sacha-backup/\\2" t)))
(setq python-shell-interpreter "/Users/patrick_yoon/.pyenv/shims/python")
;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(save))
(setq flycheck-python-flake8-executable (executable-find "python3"))
(bounds-of-thing-at-point 'symbol)


;; (defmacro after (mode &rest body)
;;   `(eval-after-load ,mode
;;      '(progn ,@body)))

(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;; use rsync
(setq tramp-default-method "rsyncc")

(setq debug-on-message "^Wrong")
(setq debug-on-error t)

(setq make-backup-files t)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(add-to-list 'backup-directory-alist
             (cons "." "~/.backups/"))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq make-backup-files t)

;; Backup (file~) disabled and auto-save (#file#) locally to prevent delays in editing remote files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))
(setq tramp-auto-save-directory temporary-file-directory)

(setq tramp-verbose 10)

(setenv "TMPDIR" "/tmp")

;; Divide files into partials.
(defvar partial-dir (expand-file-name "personal/partials" prelude-dir)
  "This directory is for your personal configuration.")
        (message "Loading personal configuration files in %s..." partial-dir)

(when (file-exists-p partial-dir)
    (message "Loading personal configuration files in %s..." partial-dir)
  (mapc 'load (directory-files partial-dir 't "^[^#].*el$")))


;; magit setup.
(global-set-key (kbd "C-c g") 'magit-status)

;; Start emacsclient server.
(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24))
      (server-start)))


;; Shows function signature in elisp mode.
(add-hook 'prelude-emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; smartparens
;; https://ebzzry.github.io/emacs-pairs.html
;; Prevent behavior of smartparens.el inserting / for quotes.
;;  http://stackoverflow.com/questions/21661737/new-to-emacs-when-i-type-is-automatically-inserted
(setq sp-autoescape-string-quote nil)

;; Define functions for wrapping with pairs.
(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

;; ;; Define key bindings. Remove bindings for C-arrow movement.
;; (bind-keys
;;  :map smartparens-mode-map
;;  ("C-M-a" . sp-beginning-of-sexp)
;;  ("C-M-e" . sp-end-of-sexp)

;;  ;; ("C-<down>" . sp-down-sexp)
;;  ;; ("C-<up>"   . sp-up-sexp)
;;  ("M-<down>" . sp-backward-down-sexp)
;;  ("M-<up>"   . sp-backward-up-sexp)

;;  ("C-M-f" . sp-forward-sexp)
;;  ("C-M-b" . sp-backward-sexp)

;;  ("C-M-n" . sp-next-sexp)
;;  ("C-M-p" . sp-previous-sexp)

;;  ("C-S-f" . sp-forward-symbol)
;;  ("C-S-b" . sp-backward-symbol)

;;  ("C-<right>" . nil)
;;  ("M-<right>" . nil)
;;  ("C-<left>"  . sp-backward-slurp-sexp)
;;  ("M-<left>"  . sp-backward-barf-sexp)

;;  ("C-M-t" . sp-transpose-sexp)
;;  ("C-M-k" . sp-kill-sexp)
;;  ("C-k"   . sp-kill-hybrid-sexp)
;;  ("M-k"   . sp-backward-kill-sexp)
;;  ("C-M-w" . sp-copy-sexp)

;;  ("C-M-d" . delete-sexp)

;;  ("M-<backspace>" . backward-kill-word)
;;  ("C-<backspace>" . sp-backward-kill-word)
;;  ([remap sp-backward-kill-word] . backward-kill-word)

;;  ("M-[" . sp-backward-unwrap-sexp)
;;  ("M-]" . sp-unwrap-sexp)

;;  ("C-x C-t" . sp-transpose-hybrid-sexp)

;;  ("C-c ("  . wrap-with-parens)
;;  ("C-c ["  . wrap-with-brackets)
;;  ("C-c {"  . wrap-with-braces)
;;  ("C-c '"  . wrap-with-single-quotes)
;;  ("C-c \"" . wrap-with-double-quotes)
;;  ("C-c _"  . wrap-with-underscores)
;;  ("C-c `"  . wrap-with-back-quotes))
;; end smartparens.

;; Custom key bindings.
;;Bind compile. \C stands for ctrl key.
(global-set-key "\C-cl" 'compile)

(global-set-key (kbd "<f5>") 'load-file) ; Load file
(global-set-key (kbd "<f6>") 'package-list-packages) ; list packages

(define-key sp-keymap (kbd "H-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "H-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-<left>") nil)
(define-key sp-keymap (kbd "C-<right>") nil)

(global-unset-key (kbd "C-<right>"))
(global-unset-key (kbd "C-<left>"))
(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "C-<left>") 'left-word)

(global-set-key (kbd "C-x a a") `ag)

;; swap to have to have similar behavior as shell.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key (kbd "<C-right>") 'right-word)
(global-set-key (kbd "<C-left>") 'left-word)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; https://github.com/purcell/exec-path-from-shell
;; ensure environment variables inside Emacs look the same as in the user's shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;;(set-default-font "Monaco 10")

;; Move point to the next occurrance of the char and can type immediately.
(global-set-key (kbd "M-m") 'iy-go-to-char)

;; Save recentf every 30 minutes. By default `recentf-save-list' is called on `kill-emacs-hook'.
(run-at-time nil (* 30 60) 'recentf-save-list)

(setq projectile-completion-system 'ido)

(pyenv-mode)

;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

(provide 'patyoon)
;;; patyoon.el ends here
