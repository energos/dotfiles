;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREFERÊNCIAS
;;
(desktop-save-mode 1)                   ; restaurar desktop,
(setq desktop-restore-frames nil)       ; sem salvar tamanho e posição dos frames

(setq inhibit-startup-screen t)         ; desabilitar 'splash screen'
(setq initial-buffer-choice t)          ; mostrar sempre *scratch* buffer no início

(scroll-bar-mode -1)                    ; desabilitar 'scrollbar'
(menu-bar-mode -1)                      ; desabilitar 'menu'
(tool-bar-mode -1)                      ; desabilitar 'tool-bar'
(tooltip-mode -1)                       ; desabilitar 'tooltips'
(blink-cursor-mode -1)                  ; cursor NÃO piscando
(line-number-mode 1)                    ; mostrar linha no 'mode line'
(column-number-mode 1)                  ; mostra coluna no 'mode-line'
(show-paren-mode 1)                     ; destacar par de parenteses
(setq frame-resize-pixelwise t)         ; pixel perfect resize
(setq default-truncate-lines t)         ; disable line wrap
(setq truncate-partial-width-windows nil)
(winner-mode 1)

(setq scroll-step 1)                    ; scroll de apenas 1 linha
(setq scroll-preserve-screen-position t); PgUp/PgDown mantém posição do cursor,
(setq scroll-error-top-bottom t)        ; exceto na primeira e última página

(defalias 'yes-or-no-p 'y-or-n-p)       ; "y or n" em vez de "yes or no"
(setq confirm-nonexistent-file-or-buffer t) ; confirmar criação de arquivo ou buffer

(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; ediff, sem frame de controle
(setq ediff-split-window-function 'split-window-horizontally) ; ediff, dividir na vertical

(defalias 'list-buffers 'ibuffer)

;; --- Themes ---
;; https://github.com/emacs-jp/replace-colorthemes
;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'charcoal-black t t)
(enable-theme 'charcoal-black)

;; --- Não expiro senhas - DANGER! ---
(setq password-cache-expiry nil)

;; --- Freaking TAB behaviour
(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode -1)

;; --- Ido mode ---
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-use-filename-at-point 'guess)
;; (ido-mode 1)
;;
;; ido vs. helm vs. counsel ?
;; let's disable ido-mode and give counsel a ride...

;; --- Mensagem inicial ---
(setq initial-scratch-message
      (let ((string "")
            (list (split-string (shell-command-to-string "fortune") "\n")))
        (dolist (line list)
          (setq string (concat string ";;" (if (= 0 (length line)) "" "  ") line "\n")))
        string))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; https://github.com/jwiegley/use-package
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

(use-package framemove
  :ensure t
  :init
  (require 'cl)
  :config
  (setq framemove-hook-into-windmove t))

(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq vc-handled-backends nil))

; https://github.com/nonsequitur/smex/
; https://github.com/abo-abo/swiper/issues/629
(use-package smex
  :ensure t)

; http://cestlaz.github.io/posts/using-emacs-6-swiper/
; https://github.com/abo-abo/swiper/blob/master/README.md
; http://oremacs.com/swiper/
(use-package counsel
  :ensure t
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c k"   . counsel-ag)
         ("C-x l"   . counsel-locate))
  :config
  (ivy-mode 1)
  ;; (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFUNs

;; https://www.emacswiki.org/emacs/SearchAtPoint#toc7
(defun backward-symbol (arg)
  "Move point to the previous position that is the beggining of a symbol."
  (interactive "p")
  (forward-symbol (- arg)))

;; https://www.emacswiki.org/emacs/NavigatingParentheses
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the opening parenthesis one level up."
  (interactive "p")
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
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD SHORTCUTS
(global-set-key (kbd "<f13>") 'ignore)
(define-key key-translation-map (kbd "<f13>") 'event-apply-hyper-modifier)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-<up>") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-<up>") (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-other-window 1)))
(global-set-key (kbd "H-<left>") 'windmove-left)
(global-set-key (kbd "H-<right>") 'windmove-right)
(global-set-key (kbd "H-<up>") 'windmove-up)
(global-set-key (kbd "H-<down>") 'windmove-down)
(global-set-key (kbd "<f12>") 'toggle-truncate-lines)
(global-set-key (kbd "C-<prior>") (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-<next>") (lambda () (interactive) (move-to-window-line -1)))
(global-set-key (kbd "C-<tab>") (lambda (arg) "Insert TAB" (interactive "P") (insert-tab arg)))
(global-set-key (kbd "M-b") 'backward-symbol)
(global-set-key (kbd "M-f") 'forward-symbol)
(global-set-key (kbd "C-c C-o") 'browse-url-at-point)
(global-set-key (kbd "M-]") 'goto-match-paren)

;; --- Mostrar arquivo correspondente ao buffer ---
(global-set-key (kbd "S-<f12>")
                (lambda () "Show current buffer file path" (interactive)
                  (or (message buffer-file-name) (message "This buffer is not a file!"))))

;; --- Recarregar buffer ---
(global-set-key (kbd "<f5>")
                (lambda () "Refresh the buffer from the disk (prompt if modified)."
                  (interactive)
                  (revert-buffer t (not (buffer-modified-p)) t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START SERVER!
(require 'server)
(unless (server-running-p)
  (server-start))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WTF!!!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (smex counsel magit framemove which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
