;;C mode setup
(setq c-basic-offset 2)
(add-hook 'prelude-c-mode-common-hook
          '(lambda ()
             (electric-pair-mode 0)
             (c-set-style "gnu")))

;; Eldoc in C mode
(add-hook 'prelude-c-mode-hook 'c-turn-on-eldoc-mode)
