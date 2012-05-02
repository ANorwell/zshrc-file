 (set-face-attribute 'default nil :height 250)

(add-to-list 'load-path "/Users/anorwell/elisp/scala-emacs")
(add-to-list 'load-path "/Users/anorwell/elisp/")
(require 'scala-mode-auto)
(require 'besi)


(mapcar (lambda (hooksym)
          (add-hook hooksym
                    (lambda ()
                      (local-set-key  (kbd "RET") 'newline-and-indent)
                      )))
        '(
          emacs-lisp-mode-hook
          java-mode-hook
          js-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          makefile-mode-hook
          nxml-mode-hook
          python-mode-hook
          ruby-mode-hook
          scheme-mode-hook
          sh-mode-hook
          ))


;;outline-minor-mode
;;keys for outline mode
(global-set-key [C-M-left] 'hide-body)
(global-set-key [C-M-right] 'show-all)
(global-set-key [M-up] 'outline-previous-heading)
(global-set-key [M-down] 'outline-next-heading)
(global-set-key [M-left] 'hide-entry)
(global-set-key [M-right] 'show-entry)
(global-set-key [C-M-up] 'outline-previous-visible-heading)
(global-set-key [C-M-down] 'outline-next-visible-heading)

;;other keys
(global-set-key [f4] 'goto-line)
(global-set-key [f8]  'grep)
(global-set-key [f10] 'menu-bar-open)
(global-set-key [f11] 'set-buffer-file-coding-system)
(global-set-key "\C-xb" 'switch-to-buffer-nocreate)
(global-set-key "\C-\M-q" 'backward-up-list-indent)

(add-hook 'cperl-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (hide-sublevels 1)
             (setq
              cperl-indent-level 4
              cperl-indent-parens-as-block t
              cperl-close-paren-offset -4)))
    

(add-hook 'scala-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(class\\|def\\|package\\|import\\|case class\\|object\\)")))

(add-hook 'php-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")
             (hide-sublevels 1)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp "^[^\s\r\t\n]")
             (hide-sublevels 1)))
    
(add-hook 'python-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
             (hide-sublevels 1)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|clas\\|require\\|describe\\|public\\|private\\)")
             (hide-sublevels 1)))
