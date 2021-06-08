;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO!
;; transparency / wallpaper
;; F11 / resize frame
;; printscreen / multimedia keys
;; remove ~/bin/pqp dependency

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exwm

;; Start some stuff
(start-process-shell-command "parcellite" nil "parcellite")
(start-process-shell-command "volumeicon" nil "volumeicon")
(start-process-shell-command "compton" nil "compton")
(start-process-shell-command "artha" nil "artha")
(start-process-shell-command "feh" nil "~/.fehbg")
(message "Loading background processes")

;; Define some stuff
;; using ~/bin/pqp as a stopgap
(defun energos/volume-up ()
  (interactive)
  (start-process-shell-command "volume-up" nil "pqp volume up"))
(defun energos/volume-down ()
  (interactive)
  (start-process-shell-command "volume-down" nil "pqp volume down"))
(defun energos/volume-toggle ()
  (interactive)
  (start-process-shell-command "volume-toggle" nil "pqp volume toggle"))
(defun energos/screen-lock ()
  (interactive)
  (start-process-shell-command "screen-lock" nil "slock"))
(defun energos/print-screen ()
  (interactive)
  (start-process-shell-command "print-screen" nil "spectacle"))
(defun energos/music-player ()
  (interactive)
  (start-process-shell-command "music-player" nil "pqp cmus"))
(defun energos/terminal ()
  (interactive)
  (start-process-shell-command "terminal" nil "pqp"))
(defun energos/new-terminal ()
  (interactive)
  (start-process-shell-command "new-terminal" nil "pqp -n"))
(defun energos/navigator ()
  (interactive)
  (start-process-shell-command "navigator" nil "firefox"))
(defun energos/new-navigator ()
  (interactive)
  (start-process-shell-command "new-navigator" nil "firefox"))

;; Hooks...
;; exwm-update-class-hook - make class name the buffer name
(defun energos/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))
;; exwm-update-title-hook - for now just send a message
(defun energos/exwm-update-title ()
  (message "Window title changed to \"%s\"." exwm-title))
;; exwm-manage-finish-hook - manage freshly created windows
(defun energos/exwm-manage-window ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))
    ("Connman-gtk" (exwm-floating-toggle-floating) (exwm-layout-toggle-mode-line))
    ("Artha" (exwm-floating-toggle-floating) (exwm-layout-toggle-mode-line))
    ("kruler" (exwm-floating-toggle-floating) (exwm-layout-toggle-mode-line))
    ("dosbox" (exwm-input--release-keyboard) (exwm-layout-toggle-mode-line))
    ("mpv" (exwm-floating-toggle-floating) (exwm-layout-toggle-mode-line)))
  (message "A new window of class %s(%s) named \"%s\" is born." exwm-class-name exwm-instance-name exwm-title))

(defun energos/workspace-switch-event ()
  (message "Workspace %d" exwm-workspace-current-index))
;; The real deal
(use-package exwm
  :ensure t
  :config

  ;; copied and fubared from exwm-config.el

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 6)

  (add-hook 'exwm-init-hook
            (lambda ()

  (add-hook 'exwm-manage-finish-hook 'energos/exwm-manage-window)
              ;; Start at workspace 1
              (exwm-workspace-switch-create 1)
              ;; yes, this is stupid, I know
              (run-with-timer 2 nil
                              (lambda ()
                                ;; waiting for a better solution...
                                (message "EXWM up and running! Now in workspace %d." exwm-workspace-current-index)))))

  (add-hook 'exwm-workspace-switch-hook 'energos/workspace-switch-event)
  (add-hook 'exwm-update-class-hook     'energos/exwm-update-class)
  (add-hook 'exwm-update-title-hook     'energos/exwm-update-title)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Global keybindings.
  (setq exwm-input-global-keys
        `(
          ([f14]     . energos/volume-down)
          ([f15]     . energos/volume-up)
          ([print]   . energos/print-screen)
          ([S-f14]   . energos/volume-toggle)
          ([S-f15]   . energos/music-player)
          ([S-print] . (lambda () (interactive) (message "You pressed the very ugly Shift+PrintScreen key")))
          ([?\s-l]   . energos/screen-lock)
          ([?\s-t]   . energos/terminal)
          ([?\s-T]   . energos/new-terminal)
          ([?\s-i]   . energos/navigator)
          ([?\s-I]   . energos/new-navigator)

          ([?\s- ]   . counsel-linux-app)
          ([?\M- ]   . counsel-linux-app)

          ;; 's-r': Reset (to line-mode).
          ([?\s-r]   . exwm-reset)

          ;; Move between windows
          ([s-left]  . windmove-left)
          ([s-right] . windmove-right)
          ([s-up]    . windmove-up)
          ([s-down]  . windmove-down)

          ;; 's-w': Switch workspace.
          ([?\s-w]   . exwm-workspace-switch)
          ;; 's-&': Launch application.
          ([?\s-&]   . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-b]   . [left])
          ([?\C-f]   . [right])
          ([?\C-p]   . [up])
          ([?\C-n]   . [down])
          ([?\C-a]   . [home])
          ([?\C-e]   . [end])
          ([?\M-v]   . [prior])
          ([?\C-v]   . [next])
          ([?\C-d]   . [delete])
          ([?\C-s]   . [?\C-f ?\C-g])
          ([?\C-k]   . [S-end delete])))

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Enable EXWM
  (exwm-enable)

  )
