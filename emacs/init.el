;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREFERÊNCIAS
;;
(desktop-save-mode 1)                   ; restaurar desktop, exceto
(setq desktop-restore-frames nil)       ; a configuração de windows e frames

(setq inhibit-startup-screen t)         ; desabilitar 'splash screen'
(setq initial-buffer-choice t)          ; iniciar mostrando o *scratch* buffer

(scroll-bar-mode -1)                    ; desabilitar 'scrollbar'
(menu-bar-mode -1)                      ; desabilitar 'menu'
(tool-bar-mode -1)                      ; desabilitar 'tool-bar'
(tooltip-mode -1)                       ; desabilitar 'tooltips'
(blink-cursor-mode -1)                  ; cursor NÃO piscando
(line-number-mode 1)                    ; mostrar linha no 'mode line'
(column-number-mode 1)                  ; mostra coluna no 'mode-line'
(show-paren-mode 1)                     ; destacar par de parenteses
(setq frame-resize-pixelwise t)         ; pixel perfect resize
(setq-default truncate-lines t)         ; disable line wrap
(setq truncate-partial-width-windows nil)
(setq save-interprogram-paste-before-kill t)
(delete-selection-mode 1)
(winner-mode 1)

(prefer-coding-system 'utf-8-unix)      ; UTF-8, no crlf, please

(setq scroll-step 1)                    ; scroll de apenas 1 linha
(setq scroll-preserve-screen-position t); PgUp/PgDown mantém posição do cursor,
(setq scroll-error-top-bottom t)        ; exceto na primeira e última página
(setq hscroll-step 1)                   ; scroll horizontal de 1 caracter,
(setq hscroll-margin 0)                 ; apenas ao chegar aos extremos da janela

(defalias 'yes-or-no-p 'y-or-n-p)       ; "y or n" em vez de "yes or no"
(setq confirm-nonexistent-file-or-buffer t) ; confirmar criação arquivo/buffer

;; ediff sem frame de controle e com janelas lado a lado
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defalias 'list-buffers 'ibuffer)

;; --- Enable some disabled commands ---
;; https://www.emacswiki.org/emacs/DisabledCommands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; --- Não expiro senhas - DANGER! ---
(setq password-cache-expiry nil)

;; --- Freaking TAB behaviour
(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode -1)

;; --- Mensagem inicial ---
(let ((command "fortune"))
  (if (executable-find command)
      (setq initial-scratch-message
            (let ((string "")
                  (list (split-string (shell-command-to-string command) "\n")))
              (dolist (line list)
                (setq string (concat string
                                     ";;"
                                     (if (= 0 (length line)) "" "  ")
                                     line
                                     "\n")))
              string))))

;; --- Tramp needs PuTTY on Windows® version of Emacs ---
;; ssh method works fine on Cygwin© and *nix Emacs
;; see https://www.gnu.org/software/tramp/
(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink"))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL elisp FILES
;;
(add-to-list 'load-path "~/.emacs.d/lisp")

;; https://emacsmirror.net/

;; framemove
;; https://github.com/emacsmirror/framemove
(require 'cl)
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; help-fns+
;; https://github.com/emacsmirror/help-fns-plus
(require 'help-fns+)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa-stbl" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stbl" .  0)
        ("gnu"        .  5)
        ("melpa"      . 10)))
(package-initialize)

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 3.0)
  (which-key-mode))

;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq vc-handled-backends nil))

;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :bind (("C-x b"   . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (setq helm-split-window-default-side 'other)
  (setq helm-candidate-number-limit 1000)
  (helm-mode 1))

;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure t
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (require 'helm)
  (require 'helm-swoop))

;; https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t)

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; http://www.nongnu.org/geiser/
(use-package geiser
  :ensure t
  :init
  (setq geiser-repl-startup-time 20000)
  (setq geiser-repl-use-other-window nil)
  (setq geiser-active-implementations '(guile racket chicken))
  (setq geiser-default-implementation 'guile))

;; https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t)

;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES

;; themes from the packages repository:
(use-package afternoon-theme    :ensure t :defer t)
(use-package ample-zen-theme    :ensure t :defer t)
(use-package blackboard-theme   :ensure t :defer t)
(use-package darkburn-theme     :ensure t :defer t)
(use-package darkmine-theme     :ensure t :defer t)
(use-package darktooth-theme    :ensure t :defer t)
(use-package eclipse-theme      :ensure t :defer t)
(use-package hc-zenburn-theme   :ensure t :defer t)
(use-package idea-darkula-theme :ensure t :defer t)
(use-package lush-theme         :ensure t :defer t)
(use-package material-theme     :ensure t :defer t)
(use-package naquadah-theme     :ensure t :defer t)
(use-package reverse-theme      :ensure t :defer t)
(use-package tangotango-theme   :ensure t :defer t)
(use-package zenburn-theme      :ensure t :defer t)

;; hand picked themes:
;; https://github.com/emacs-jp/replace-colorthemes
;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; choose one of them:
;; (load-theme 'charcoal-black t t)
;; (enable-theme 'charcoal-black)
(load-theme 'afternoon t t)
(enable-theme 'afternoon)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFUNs

;; https://www.emacswiki.org/emacs/NavigatingParentheses
(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis.
Else go to the opening parenthesis one level up."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

;; http://emacsblog.org/2007/01/17/indent-whole-buffer/
;; indent whole buffer, remove trailing spaces
(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun energos/inc-or-dec (n max &optional dec)
  "Increment or decrement N, limiting the result to the interval 0≤N≤MAX.
If DEC is t:             Return N-1 if 0<N≤MAX, 0 if N≤0, MAX if N>MAX.
If DEC is nil or absent: Return N+1 if 0≤N<MAX, 0 if N<0, MAX if N≥MAX."
  (or (integerp n) (setq n 0))                ; must be an integer
  (setq n (or (and dec (1- n)) (1+ n)))       ; increment or decrement
  (or (and (< n 0) 0) (and (> n max) max) n)) ; limit to the valid range

(defun energos/resize-frame (&optional dec)
  "If DEC is t, decrease current frame size, else increase current frame size."
  (interactive "P")
  (let* ((list [84 170 244])
         (i (energos/inc-or-dec
             (frame-parameter (selected-frame) 'energos/width)
             (1- (length list)) dec))
         (width (aref list i)))
    (set-frame-parameter (selected-frame) 'energos/width i)
    (set-frame-width (selected-frame) width)
    (message (format "Frame width resized to %d characters" width))))

;; --- Recarregar o Browser ---
;; https://addons.mozilla.org/en-US/firefox/addon/remote-control
(defun browser-reload ()
  "Reload browser."
  (interactive)
  (shell-command "echo reload | nc -q 1 localhost 32000 && echo Browser Reloaded"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files '("~/org/notes.org" "~/org/agenda.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . nil)
   (dot . t)
   (calc . t)
   (shell . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (ruby . t)
   (C . t)
   (python . t)
   (scheme . t)
   ))

;; Avaliar blocos de código sem exigir confirmação
;; http://orgmode.org/manual/Code-evaluation-security.html
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (or
              ;; t                        ; não pergunto nada
              (string= lang "ditaa")      ; não pergunto por ditaa
              (string= lang "dot")        ; ...
              (string= lang "calc")
              (string= lang "emacs-lisp")
              (string= lang "C")
              (string= lang "C++")
              (string= lang "sh")         ; https://emacs.stackexchange.com/questions/35321/
              (string= lang "shell")      ;
              (string= lang "ruby")
              (string= lang "python")
              (string= lang "scheme")
              ))))

;; https://emacs.stackexchange.com/questions/2387/browser-not-opening-when-exporting-html-from-org-mode
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . "xdg-open %s")
        ("\\.pdf\\'" . "xdg-open %s")))

;; --- preferences ---
(setq org-replace-disputed-keys t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-link-frame-setup
      (quote
       ((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame))))
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(setq org-export-coding-system 'utf-8)

;; --- org-refile ---
;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-targets '((nil . (:maxlevel . 6)) (org-agenda-files . (:maxlevel . 6))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; --- org-capture ---
(require 'org-protocol)
(require 'org-capture)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %u %?" :prepend t)
        ("j" "Journal entry" entry (file+datetree "journal.org")
         "* %U %^{Title}\n  %?")
        ("n" "Note" entry (file+headline org-default-notes-file "Unsorted Notes")
         "* %?\n  %i\n  %a")
        ))
;; See org-capture-templates help:
;; %?          After completing the template, position cursor here.
;;
;; %i          Initial content, copied from the active region.  If %i is
;;             indented, the entire inserted text will be indented as well.
;;
;; %a          Annotation, normally the link created with `org-store-link'.
;; %t          Time stamp, date only.
;; %T          Time stamp, with date and time.
;; %u, %U      Like the above, but inactive time stamps.

;; --- org-mode specific shortcuts ---
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD SHORTCUTS
(global-set-key (kbd "<f13>") 'ignore)
(define-key key-translation-map (kbd "<f13>") 'event-apply-hyper-modifier)
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (setq w32-enable-caps-lock nil)
  (define-key key-translation-map (kbd "<capslock>") 'event-apply-hyper-modifier)
  (message "Windows® system detected")
  )

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-<up>") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-<left>") (lambda () (interactive) (scroll-right 1)))
(global-set-key (kbd "C-<right>") (lambda () (interactive) (scroll-left 1)))

(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (scroll-other-window 1)))

(windmove-default-keybindings 'hyper)

(global-set-key (kbd "<f12>") 'toggle-truncate-lines)

(global-set-key (kbd "C-<prior>")
                (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-<next>")
                (lambda () (interactive) (move-to-window-line -1)))

(global-set-key (kbd "C-<tab>")
                (lambda (arg) "Insert TAB." (interactive "p") (insert-tab arg)))

;; https://www.emacswiki.org/emacs/SearchAtPoint#toc7
(global-set-key (kbd "M-b") (lambda (arg) "\
Move point to the previous position that is the beggining of a symbol."
                              (interactive "p") (forward-symbol (- arg))))
(global-set-key (kbd "M-f") 'forward-symbol)

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)
(setq browse-url-browser-function 'browse-url-xdg-open)

(global-set-key (kbd "M-]") 'goto-match-paren)

;; --- Mostrar arquivo correspondente ao buffer ---
(global-set-key (kbd "S-<f12>")
                (lambda () "Show current buffer file path."
                  (interactive)
                  (or (message buffer-file-name)
                      (message "This buffer is not a file!"))))

;; --- Recarregar buffer ---
(global-set-key (kbd "<f5>")
                (lambda () "Reload buffer from disk if modified."
                  (interactive)
                  (revert-buffer t (not (buffer-modified-p)) t)))

;; --- Frame resize ---
(global-set-key (kbd "<f11>") 'energos/resize-frame)
(global-set-key (kbd "S-<f11>")
                (lambda () (interactive) (energos/resize-frame t)))

(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

;; --- Frequently used files ---
(global-set-key (kbd "\e\ei") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "\e\en") (lambda () (interactive) (find-file org-default-notes-file)))

;; --- CapsLock -> F13 -> Hyper ---
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-b") 'helm-buffers-list)
(global-set-key (kbd "H-f") 'helm-find-files)
(global-set-key (kbd "H-c") (kbd "C-c C-c"))
(global-set-key (kbd "H-e") (kbd "C-x C-e"))
(global-set-key (kbd "H-<f13>") (kbd "C-x C-e"))
(global-set-key (kbd "H-d") 'geiser-doc-symbol-at-point)
(global-set-key (kbd "H-m") 'magit-status)
(global-set-key (kbd "H-Q") 'save-buffers-kill-terminal)
(global-set-key (kbd "H-n") 'make-frame-command)

(global-set-key (kbd "H-g")
                (lambda () "Set C-x C-e to call geiser-eval-last-sexp."
                  (interactive)
                  (progn (local-set-key (kbd "C-x C-e") 'geiser-eval-last-sexp)
                         (message "C-x C-e will call geiser-eval-last-sexp"))))
(global-set-key (kbd "H-G")
                (lambda () "Set C-x C-e to call the default command."
                  (interactive)
                  (progn (local-unset-key (kbd "C-x C-e"))
                         (message "C-x C-e reset to default"))))

(global-set-key (kbd "H-0") 'delete-window)
(global-set-key (kbd "H-1") 'delete-other-windows)
(global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-3") 'split-window-horizontally)

;; --- Support for keyboard physical macro keys ---
;;
;; M1 - ESC <f1> -> C-c &
;; M2 - ESC <f2> -> C-c C-o
;; M3 - ESC <f3> -> C-c C-c
;; M4 - ESC <f4> -> C-x C-e
;; M5 - ESC <f5> -> C-x

;; m1 map
(define-prefix-command 'energos/m1-map)
(define-key energos/m1-map (kbd "<f1>") (kbd "C-x C-e"))
(define-key energos/m1-map (kbd "<f2>") (kbd "C-x C-e"))
(define-key energos/m1-map (kbd "<f3>") (kbd "C-x C-e"))
(define-key (current-global-map) (kbd "H-<escape>") energos/m1-map)

;; M1
(define-key key-translation-map (kbd "ESC <f1>") (kbd "C-c &"))

;; M2
(define-key key-translation-map (kbd "ESC <f2>") (kbd "C-c C-o"))

;; M3
(define-key key-translation-map (kbd "ESC <f3>") (kbd "C-c C-c"))

;; M4
(define-key key-translation-map (kbd "ESC <f4>") (kbd "C-x C-e"))

;; M5
(define-key key-translation-map (kbd "ESC <f5>") (kbd "C-x"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START SERVER!
(require 'server)
(unless (server-running-p)
  (server-start))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZE
;; Keep this file free from customize data
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
