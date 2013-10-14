;; use el-get for package manager
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
;; list all packages that I want

;; Turn off icomplete, use helm instead
(icomplete-mode 99)

;; ;; disable whitespace mode showing whitespaces as dots.
;; (setq prelude-whitespace nil)

;; use arrow navigatioin in editor buffers..for now.
(defun disable-guru-mode ()
  (guru-mode -1)
)
(setq prelude-guru nil)
(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; use ido-ubiquitous mode.
;; ido-style completion for (almost) every function that uses the standard
;; completion function "completing-read"
;; https://github.com/technomancy/ido-ubiquitous
(ido-ubiquitous 1)
'(ido-enable-last-directory-history nil)
'(ido-enable-regexp nil)
'(ido-max-directory-size 300000)
'(ido-max-file-prompt-width 0.1)
'(ido-use-filename-at-point (quote guess))
'(ido-use-url-at-point t)

;; Smex is a M-x enhancement for Emacs.
;; http://www.emacswiki.org/emacs/Smex
(require 'smex)
(setq smex-history-length 100)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Use Helm for buffer switching and file finding
;;(global-set-key (kbd "\C-x b") 'helm-buffers-list)
;;(global-set-key (kbd "\C-x \C-f") 'helm-find-files)

;; Recentf is a minor mode that builds a list of recently opened files.
;; http://www.emacswiki.org/emacs/RecentFiles
(add-to-list 'recentf-exclude "\\.ido\\.last")
(add-to-list 'recentf-exclude "recentf")
(global-set-key (kbd "\C-x f") 'prelude-recentf-ido-find-file)
;; C-x b recent file finding. This feature relies upon the recentf package.
(setq ido-use-virtual-buffers 1)

;; Turn on linum mode
(global-linum-mode t)
(setq linum-format "%d ")

;; for fixing broken fringes issue
;;(fringe-mode 0)

;;C mode setup
(setq c-basic-offset 2)
(add-hook 'prelude-c-mode-common-hook
          '(lambda ()
             (electric-pair-mode 0)
             (c-set-style "gnu")))

;; Eldoc in C mode
(add-hook 'prelude-c-mode-hook 'c-turn-on-eldoc-mode)

;; ;;Command for sending to email (bound to C-c m)
;; (defun mail-current-buffer ()
;;   "Send the current buffer to email (for Mac)"
;;   (interactive)
;;   (shell-command (format "open -a Sparrow %s" (buffer-file-name))))
;; (define-key global-map "\C-cm" 'mail-current-buffer)

;;Make lines wrap instead of going over edge
(global-visual-line-mode)

;;Bind compile
(global-set-key "\C-cl" 'compile)

; From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
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

;; Reindent on yank
;; http://emacswiki.org/emacs/AutoIndentation
;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (member major-mode '(emacs-lisp-mode lisp-mode
;;                                                      clojure-mode    scheme-mode
;;                                                      ruby-mode
;;                                                      rspec-mode
;;                                                      c-mode          c++-mode
;;                                                      objc-mode       latex-mode
;;                                                      plain-tex-mode java-mode))
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (indent-region (region-beginning) (region-end) nil))))))

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
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay .3)
(ac-set-trigger-key "TAB")
;; Make \C-n and \C-p work in autocompletion menu
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;;More package archives
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Buffer movement by arrow keys
;; (require 'buffer-move)
;; (global-set-key (kbd "<up>")     'buf-move-up)
;; (global-set-key (kbd "<down>")   'buf-move-down)
;; (global-set-key (kbd "<left>")   'buf-move-left)
;; (global-set-key (kbd "<right>")  'buf-move-right)

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

;;Email setup
;; (setq user-mail-address "kinetoz@gmail.com")
;;       (setq user-full-name "Patrick Yoon")
;; (setq smtpmail-smtp-user "kinetoz")
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq mail-user-agent 'message-user-agent)
;; (setq message-send-mail-function 'smtpmail-send-it)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq backup-by-copying t)
(global-subword-mode 1) ;move thru camelCaseWords

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; ;; swap to have to have similar behavior as shell.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;;hyde
(defun hyde-new-draft()
  "Init a new blog post for hyde engine"
  (interactive)
  (insert "{% extends \"_post.html\" %}\n{%hyde\n   title:\n   categories:\n   created: ")
  (insert (format-time-string "%Y-%m-%d"))
  (insert "\n%}\n{% block article %}\n{% article %}\n\n{% endarticle %}\n{% endblock %}")
  (goto-line 9)
  )

;;(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
;;(autoload 'haskell-latex-mode "haskell-latex")

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

;; python-mode

;; python mode tab width
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(add-to-list 'el-get-recipe-path (expand-file-name "~/prg/el-get/recipes"))

(require 'package)
(package-initialize)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path "~/.emacs.d/vendor/arduino")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

(setq TeX-PDF-mode t)
;; set alt keys for meta
(setq mac-option-modifier 'meta)
;; only works in cocoa version
;;(mac-key-mode 1)

;; (setq-default indent-tabs-mode nil)
;; ;; tabs are alwasy 2 spaces at TellApart
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
;;no popup when opening buffer from terminal
(setq ns-pop-up-frames nil)

;; ;;;;;; From Jesh's

;; ;; Show trailing whitespace
;; (require 'whitespace)
;; (setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
;; (custom-set-variables '(show-trailing-whitespace t))

;; ;; ;; Highlight TODO, FIXME, and lines longer than 80 chars
;; ;; (make-face 'long-line-face)
;; ;; (set-face-background 'long-line-face "#452828")
;; ;; (defvar long-line-face 'long-line-face)
;; ;; (defun sourcecode_buffers ()
;; ;;   "Returns non-nil if current mode corresponds to source code."
;; ;;   (memq major-mode '(c++-mode
;; ;;                      c-mode
;; ;;                      javascript-mode
;; ;;                      python-mode
;; ;;                      sawzall-mode
;; ;;                      perl-mode
;; ;;                      makefile-mode
;; ;;                      emacs-lisp-mode
;; ;;                      nxml-mode)))
;; ;; (defun add-fixme-highlighting ()
;; ;;   "Turn on extra highlighting for 'FIXME' and 'TODO'."
;; ;;   (if (sourcecode_buffers)
;; ;;       (progn
;; ;;         (font-lock-add-keywords
;; ;;          nil
;; ;;          '(("\\<\\(FIXME\\|TODO\\|NOTE\\|XXX+\\)" 0 font-lock-warning-face
;; ;;             prepend)))
;; ;;         (font-lock-add-keywords
;; ;;          nil
;; ;;          '(("^.\\{81\\}\\(.+\\)$" 1 'long-line-face append)))
;; ;;         (font-lock-add-keywords
;; ;;          nil
;; ;;          '((",[^ \n]" 0 'long-line-face append)))
;; ;;         )
;; ;;     )
;; ;;   )
;; ;; (add-hook 'font-lock-mode-hook 'add-fixme-highlighting)

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
(setq js-indent-level 2)

;; ;-----------------------------
;; ; Python Specifics
;; ;-----------------------------
;; ;(add-to-list 'load-path "/Users/jeshua/config/emacs/elpy-0.9/")
;; ;(load "/Users/jeshua/config/emacs/elpy-0.9/elpy-autoloads.el")
;; ;(package-initialize)
;; ;(elpy-enable)
;; ;(elpy-use-ipython)
;; ;(elpy-clean-modeline)
;; ;(auto-complete-mode)

;; ;; (require 'package)
;; ;; (add-to-list 'package-archives
;; ;;     '("marmalade" .
;; ;;       "http://marmalade-repo.org/packages/"))
;; ;; (package-initialize)
;; ;; (add-to-list 'package-archives
;; ;;   '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ;; ;(defun my-flymake-show-help ()
;; ;; ;  (when (get-char-property (point) 'flymake-overlay)
;; ;; ;   (let ((help (get-char-property (point) 'help-echo)))
;; ;; ;    (if help (message "%s" help)))))

;; ;; ;(add-hook 'post-command-hook 'my-flymake-show-help)

;; Instead of python.el, use python-mode.el that supports ipython.
;;(require 'python-mode)

;; yas with ido-ubiquotous
  ;; Completing point by some yasnippet key
  ;; (defun yas-ido-expand ()
  ;;   "Lets you select (and expand) a yasnippet key"
  ;;   (interactive)
  ;;     (let ((original-point (point)))
  ;;       (while (and
  ;;               (not (= (point) (point-min) ))
  ;;               (not
  ;;                (string-match "[[:space:]\n]" (char-to-string (char-before)))))
  ;;         (backward-word 1))
  ;;     (let* ((init-word (point))
  ;;            (word (buffer-substring init-word original-point))
  ;;            (list (yas-active-keys)))
  ;;       (goto-char original-point)
  ;;       (let ((key (remove-if-not
  ;;                   (lambda (s) (string-match (concat "^" word) s)) list)))
  ;;         (if (= (length key) 1)
  ;;             (setq key (pop key))
  ;;           (setq key (ido-completing-read "key: " list nil nil word)))
  ;;         (delete-char (- init-word original-point))
  ;;         (insert key)
  ;;         (yas-expand)))))

  ;;     (define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

;;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; (require 'python-pep8)
;; (require 'python-pylint)

;; zshrc does not affect carbon emacs.

;;set ipython as default python shell
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

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
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)


;; ipython notebook
;;(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default python-indent 2)
(setq-default py-indent-offset 2)
(setq py-indent  2)
(setq py-indent-offset  2)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ;;pdb setup, note the python version
;; (setq pdb-path '/usr/lib/python2.6/pdb.py
;;       gud-pdb-command-name (symbol-name pdb-path)
;;       (defadvice pdb (before gud-query-cmdline activate)
;;         "Provide a better default command line when called interactively."))
;; (interactive
;;  (list (gud-query-cmdline pdb-path
;;                           (file-name-nondirectory buffer-file-name))))

;; tramp to use server nicknames
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

(global-set-key (kbd "<C-right>") 'right-word)
(global-set-key (kbd "<C-left>") 'left-word)

(toggle-debug-on-error 1)
;; smart parens mode sucks!
;;(smartparens-global-mode -1)
;;(smartparens-mode -1)

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

;;(require 'ipython)
(setq python-python-command "/usr/bin/python2.7")

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(save))

    (setenv "PATH"
            (concat
             "~/.virtualenvs/default/bin" ":"
             (getenv "PATH")
             ))

;; ;;rope

(require 'pymacs)
  (setenv "PYMACS_PYTHON" "python2.7")

   (defun load-ropemacs ()
      "Load pymacs and ropemacs"
      (interactive)
      (setenv "PYMACS_PYTHON" "python2.6")
      (require 'pymacs)
      (pymacs-load "ropemacs" "rope-")

      ;; ropemacs variables
      (setq ropemacs-confirm-saving 'nil)
      (setq ropemacs-enable-autoimport 't)
      (setq ropemacs-autoimport-modules '("os" "shutil"))
      (setq ropemacs-autoimport-underlineds 't)
    )

  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
(load-ropemacs)
  (setq ropemacs-enable-autoimport t)

;; virtualenv support
  (push "~/.virtualenvs/default/bin" exec-path)


  ; use IPython
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython")
  ; use the wx backend, for both mayavi and matplotlib
  (setq py-python-command-args
    '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
  (setq py-force-py-shell-name-p t)

  ; switch to the interpreter after executing code
  (setq py-shell-switch-buffers-on-execute-p t)
  (setq py-switch-buffers-on-execute-p t)
  ; don't split windows
  (setq py-split-windows-on-execute-p nil)
  ; try to automagically figure out indentation
  (setq py-smart-indentation t)

  (provide 'patyoon)
  ;;; patyoon.el ends here
