;;; Code: My emacs setup
;;; use el-get for package manager

;; use el-get for manaing packages.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; list all packages that I want
(setq my-el-get-packages
      (append
       '(gist
         anything
         yasnippet
         zenburn
         yaml-mode
         yas-jit
         flx
         backup-each-save
         virtualenvwrapper
         sql-complete
         sass-mode
         auto-complete
         coffee-mode
         cython-mode
         django-mode
         haskell-latex
         helm
         ido-ubiquitous
         jinja2-mode
         jekyll-el
         magit
         markdown-mode
         package
         popup
         rope
         ropemacs
         pymacs
         pos-tip
         popup-pos-tip
         smex
         dired+
         jedi
         dired-details
         find-dired+
         company-mode
         ;; Offline  API Documentation Browser and Code Snippet Manager.
         ;; https://github.com/stanaka/dash-at-point#readme
         dash-at-point
         ;; highlight-indentations
         ;; https://github.com/antonj/Highlight-Indentation-for-Emacs
         highlight-indentation
         ;; http://emacsrocks.com/e13.html
         multiple-cursors
         )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

;; Turn off icomplete, use helm instead.
(icomplete-mode 99)

;; Disable whitespace mode showing whitespaces as dots.
;; (setq prelude-whitespace nil)

;; use arrow navigation for now.
(defun disable-guru-mode ()
  (guru-mode -1)
  )

(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; use ido-ubiquitous mode.
;; ido-style completion for (almost) every function that uses the standard
;; completion function "completing-read".
;; https://github.com/technomancy/ido-ubiquitous
(require 'ido)
(ido-mode 1)
(ido-ubiquitous 1)
'(ido-enable-last-directory-history nil)
'(ido-enable-regexp nil)
'(ido-max-directory-size 300000)
'(ido-max-file-prompt-width 0.1)
'(ido-use-filename-at-point (quote guess))
'(ido-use-url-at-point t)

;; M-x enhancement.
;; http://www.emacswiki.org/emacs/Smex
(require 'smex)
(setq smex-history-length 100)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; A minor mode that builds a list of recently opened files.
;; http://www.emacswiki.org/emacs/RecentFiles
(add-to-list 'recentf-exclude "\\.ido\\.last")
(add-to-list 'recentf-exclude "recentf")
(global-set-key (kbd "\C-x f") 'prelude-recentf-ido-find-file)
;; C-x b recent file finding. This feature relies upon the recentf package.
(setq ido-use-virtual-buffers 1)

;; Turn on linum mode
(global-linum-mode t)
(setq linum-format "%d ")

;; for fixing broken fringe-mode issue
(set-fringe-mode '(0 . 0))

;;Make lines wrap instead of going over edge
(global-visual-line-mode)

;;Bind compile.
(global-set-key "\C-cl" 'compile)

;; From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (if (not (memq last-command '(
                                set-mark
                                cua-set-mark
                                goto-match-paren
                                down-list
                                up-list
                                end-of-defun
                                beginning-of-defun
                                backward-sexp
                                forward-sexp
                                backward-up-list
                                forward-paragraph
                                backward-paragraph
                                end-of-buffer
                                beginning-of-buffer
                                backward-word
                                forward-word
                                mwheel-scroll
                                backward-word
                                forward-word
                                mouse-start-secondary
                                mouse-yank-secondary
                                mouse-secondary-save-then-kill
                                move-end-of-line
                                move-beginning-of-line
                                backward-char
                                forward-char
                                scroll-up
                                scroll-down
                                scroll-left
                                scroll-right
                                mouse-set-point
                                next-buffer
                                previous-buffer
                                previous-line
                                next-line
                                )
                 ))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))
(global-set-key (kbd "%") 'goto-match-paren)

;; Delete extra whitespace when killing lines
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
    Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; Elisp useful stuff
(add-hook 'prelude-emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;  provides the function for displaying a tooltip at mouse
;;  position which allows users to easily show it.
;;(require 'pos-tip)
;;(require 'popup-pos-tip)
;; (defadvice popup-tip
;; (around popup-pos-tip-wrapper (string &rest args) activate)
;; (if (eq window-system 'x)
;;              (apply 'popup-pos-tip string args)
;;          ad-do-it))

;;Autocomplete mode nicer autocomplete rendering
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
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;;More package archives
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;;CMake mode
;;(require 'cmake-mode)
;; (setq auto-mode-alist
;;       (append '(("CMakeLists\\.txt\\'" . cmake-mode)
;;                 ("\\.cmake\\'" . cmake-mode))
;;               auto-mode-alist))
;; (setq auto-mode-a)

;;Disable flymake GUI warnings (they cause crash)
(setq flymake-gui-warnings-enabled nil)

;; Scroll to end of window before error
(setq scroll-error-top-bottom t)

;; Ido-ubiquitous broken in M-x man, disable it
;; (add-to-list 'ido-ubiquitous-command-exceptions 'man)
;; (ido-ubiquitous-disable-in man)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq backup-by-copying t)
(global-subword-mode 1) ;move thru camelCaseWords

;; fontify code in code blocks
;;(setq org-src-fontify-natively t)

;; swap to have to have similar behavior as shell.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
(autoload 'haskell-latex-mode "haskell-latex")

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50)
                (setq i (1+ i))) (next-buffer))

    (defun previous-user-buffer ()
      "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
      (interactive)
      (previous-buffer)
      (let ((i 0))
        (while (and (string-match "^*" (buffer-name)) (< i 50))
          (setq i (1+ i)) (previous-buffer))))))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

;; sample easy keys
(global-set-key (kbd "<f5>") 'load-file) ; Load file
(global-set-key (kbd "<f6>") 'package-list-packages) ; list packages

(global-set-key (kbd "<C-prior>") 'previous-user-buffer) ; Ctrl+PageUp
(global-set-key (kbd "<C-next>") 'next-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "<C-S-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
(global-set-key (kbd "<C-S-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown
(custom-set-variables

 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "50be9f1476216575cabae8717557fca94642f8ca9c72b4bae09b9f1606233c67" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; for evernote mode
(setq evernote-username "patryoon") ; optional: you can use this username as default.
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(add-to-list 'el-get-recipe-path (expand-file-name "~/prg/el-get/recipes"))

(require 'package)
(package-initialize)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq TeX-PDF-mode t)
;; set alt keys for meta
(setq mac-option-modifier 'meta)
;; only works in cocoa version
;; (mac-key-mode 1)

;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
;;no popup when opening buffer from terminal
(setq ns-pop-up-frames nil)

(defun remove-trailing-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))
(add-hook 'write-file-hooks 'remove-trailing-whitespace)

;; Indentation
(setq default-tab-width 2)
(setq c-basic-offset 2)

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

(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; tramp to use server nicknames
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

(global-set-key (kbd "<C-right>") 'right-word)
(global-set-key (kbd "<C-left>") 'left-word)
(turn-off-smartparens-mode)
(toggle-debug-on-error 1)
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

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(save))
(bounds-of-thing-at-point 'symbol)

;; (defmacro after (mode &rest body)
;;   `(eval-after-load ,mode
;;      '(progn ,@body)))

;; (after 'auto-complete
;;        (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;;        (setq ac-use-menu-map t)
;;        (define-key ac-menu-map "\C-n" 'ac-next)
;;        (define-key ac-menu-map "\C-p" 'ac-previous))

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

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)
;;  (defvar backup-each-save-mirror-location "~/.backups")

(define-key sp-keymap (kbd "H-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "H-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-<left>") nil)
(define-key sp-keymap (kbd "C-<right>") nil)

(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "C-<left>") 'left-word)

(global-set-key (kbd "C-x a a") `ag)

;; Divide files into partials.
;; Does not seem to work..
(defvar partial-dir (expand-file-name "personal/partials" prelude-dir)
  "This directory is for your personal configuration.")
        (message "Loading personal configuration files in %s..." partial-dir)

(when (file-exists-p partial-dir)
    (message "Loading personal configuration files in %s..." partial-dir)
  (mapc 'load (directory-files partial-dir 't "^[^#].*el$")))

;;(push 'company-robe company-backends)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; magit setup.
(global-set-key (kbd "C-c g") 'magit-status)

;; Start emacsclient server.
(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24))
      (server-start)))

(provide 'patyoon)
;;; patyoon.el ends here
