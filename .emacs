(defvar *aquamacs-p* (boundp 'aquamacs-version))
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(set-face-attribute 'default nil :height 150)
(global-auto-revert-mode t)
(column-number-mode t)
(scroll-bar-mode -1)

;;backup to this dir
(setq backup-directory-alist `(("." . "~/.emacs-backup")))

 (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

;;paren mode (parens matching)
(require 'paren)
(show-paren-mode 1)
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;;(add-to-list 'load-path "/Users/anorwell/elisp/scala-emacs")
(add-to-list 'load-path "/Users/anorwell/elisp/")
;;(require 'scala-mode-auto)
;;(require 'besi)

;;theming
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(setq custom-theme-load-path '(custom-theme-directory t))

;;code syntax highlighting for org-mode
(setq org-src-fontify-natively t)

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

(setq ensime-sem-high-faces
  '(
   (var . (:foreground "#ffc9c9"))
   (val . (:foreground "#dddddd"))
   (varField . (:foreground "#ffbbbb"))
   (valField . (:foreground "#ccccff"))
   (functionCall . (:foreground "#84BEE3"))
   (param . (:foreground "#ffff99"))
   (class . (:foreground "#66CC66"))
   (trait . font-lock-type-face)
   (object . (:foreground "#22EE22"))
   (package . font-lock-preprocessor-face)
   ))


;;ido-mode
(require 'ido)
(ido-mode t)

;;whitespace
 (require 'whitespace)
 (setq whitespace-style '(face empty tabs trailing))
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
  (switch-to-buffer (find-file-noselect
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

(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer. 
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))

;;(define-key reb-mode-map "\C-c\M-%" 'reb-query-replace-this-regxp)


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

;; smex for meta
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))


;;;;;; ediff mode diffing binary files

(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff 
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ediff binary file stuff
;;;;;;;;;;;;;;


(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes (quote ("36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(ido-enable-flex-matching t)
 '(show-paren-mode t)
 '(show-paren-priority 1000)
 '(show-paren-style (quote parenthesis))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "DejaVu_Sans_Mono"))))
 '(show-paren-match ((t (:background "#770077" :foreground "#259188" :inverse-video nil :underline nil :slant normal :weight bold))))
 '(whitespace-empty ((t (:background "yellow"))))
 '(whitespace-line ((t (:background "#001818")))))

;;use solarized theme
;;(load-theme 'solarized-dark)

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(server-start)
