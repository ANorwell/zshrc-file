(defvar *aquamacs-p* (boundp 'aquamacs-version))
(when *aquamacs-p*
(tool-bar-mode -1))
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(set-face-attribute 'default nil :height 250)
(global-auto-revert-mode t)

(add-to-list 'load-path "/Users/anorwell/elisp/scala-emacs")
(add-to-list 'load-path "/Users/anorwell/elisp/")
(require 'scala-mode-auto)
(require 'besi)

;;ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("conf" (name . "conf$"))
               ("adm-test" (filename . "work/adm4/src/test"))
               ("adm" (filename ."work/adm4"))
               ("oldadm-test" (filename . "work/adm/src/test"))
               ("oldadm" (filename ."work/adm"))
               ("starburst-test" (filename . "work/analytics/src/test"))
               ("starburst" (filename ."work/analytics"))
               ("cavalry-test" (filename . "work/cavalry/src/test"))
               ("cavalry" (filename ."work/cavalry"))
               ("smoke-test" (filename . "work/smoke/src/test"))
               ("smoke" (filename ."work/smoke"))

               ("other-scala-test" (filename . "src/test.*scala$"))
               ("other-scala" (name . "scala$"))
               ("dired" (mode . dired-mode))
               ("perl" (mode . cperl-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))


;;ido-mode
(require 'ido)

;;whitespace
 (require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)

;;ensime-mode for scala
(add-to-list 'load-path "~/misc/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;before-save hook for scala
(add-hook 'scala-mode-hook
     (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;;markdown mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;newline and indent for various modes
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
             (setq outline-regexp "[\s\r\t]*\\(class\\|def\\|package\\|import\\|case class\\|object\\|trait\\|abstract\\|mixin\\|protected def\\|sealed\\|override\\|private def\\|describe\\|it(\\)")))

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

(defun spec-buffer-switch ()
  "Switch to/from the Spec file in the test folder of a scala/maven
project"
  (interactive)
  (switch-to-buffer-other-window (find-file-noselect
   (if (string-match "Spec.scala$" (buffer-file-name))
(concat
  (substring (replace-regexp-in-string "test" "main"
      (buffer-file-name)) 0 -10)
  ".scala")
(concat
  (substring (replace-regexp-in-string "main" "test"
      (buffer-file-name)) 0 -6)
  "Spec.scala")))))

(global-set-key (kbd "C-,") 'spec-buffer-switch)


(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))
