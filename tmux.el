;;; tmux.el --- A tool for managing tmux sessions. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient s)

(defvar tmux-buffer-list '())

(defvar tmux-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-i"        'tmux/popup) ; tab
    (define-key map "c"           'tmux/send)
    map)
  "Keymap for Tmux Mode.")

;;;###autoload
(define-derived-mode tmux-session-mode special-mode "Tmux Session Mode"
  "Mode for interacting with Tmux sessions."
  (buffer-disable-undo)
  (defvar-local tmux-session-name 'nil)
  (setq-local revert-buffer-function
	      'tmux/refresh-buffer)
  (setq-local tmux-refresh-timer
	      'nil)

  ;; add buffer to buffer list
  (add-to-list 'tmux-buffer-list
	       (current-buffer))
  ;; remove buffer from list when it's killed
  ;; (add-hook 'kill-buffer-hook
  ;; 	    (lambda ()
  ;; 	      (delete (current-buffer) tmux-buffer-list)))

  ;; make keymap
  (evil-make-overriding-map tmux-session-mode-map 'normal))

(transient-define-prefix tmux/popup ()
  "Tmux Commands"
  [:description (lambda () (if tmux-refresh-timer
			  (format "Current-session: %s, Auto Refresh Enabled" tmux-session-name)
			(format "Current-session: %s" tmux-session-name)))
		["Sessions"
		 ("s" "Switch Session" tmux/switch-session)
		 ("n" "New Session" tmux/new-session)
		 ("k" "Kill Session" tmux/kill-session)]
		["Input/Output"
		 ("r" "Refresh" tmux/refresh-buffer)
		 ("c" "Command" tmux/send)
		 ("t" "Timer" tmux/toggle-refresh-timer)]])

;; Commands

(defun tmux/switch-session ()
  (interactive)
  (with-current-buffer (current-buffer)
    (setq-local tmux-session-name (tmux/get-session))
    (tmux/capture-pane)))

(defun tmux/new-session ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((name (read-string "Session name: ")))
      (tmux/command (format "new -d -s %s" name))
      (setq-local tmux-session-name name)
      (tmux/capture-pane))))

(defun tmux/kill-session ()
  (interactive)
  (with-current-buffer (current-buffer)
    (if (and tmux-session-name
	     (yes-or-no-p (format "Are you sure you want to kill '%s'?" tmux-session-name)))

	(progn (tmux/command (format "kill-pane -t %s" tmux-session-name))
	       (tmux/switch-session))
      (error "No tmux session."))))

(defun tmux/refresh-buffer (&optional ignore-auto no-confirm)
  (interactive)
  (tmux/capture-pane))

(defun tmux/send ()
  (interactive)
  (with-current-buffer (current-buffer)
    (if tmux-session-name
	(let* ((cmd (read-string "Command: ")))
	  (tmux/command (format "send -t %s '%s' ENTER"
				tmux-session-name
				cmd))
	  (tmux/capture-pane))
      (error "No tmux session."))))

(defun tmux/toggle-refresh-timer ()
  (interactive)
  (with-current-buffer (current-buffer)
    (if tmux-refresh-timer
	(progn (cancel-timer tmux-refresh-timer)
	       (setq-local tmux-refresh-timer 'nil))
      (setq-local tmux-refresh-timer
		  (run-with-idle-timer 5 t 'tmux/refresh-all-buffers)))))

;; Helpers

(defun tmux/get-session ()
  "Interactively get running session."
  (let* ((raw-sessions (tmux/command "list-sessions"))
	 (names (--map
		 (car (s-split ":" it))
		 (--filter
		  (not (s-equals? "" it))
		  (s-split "\n" raw-sessions)))))
    (cond
     ((eq (length names) 0) (error "No live sessions."))
     ((eq (length names) 1) (car names))
     (t (completing-read "Sessions: " names nil t)))))

(defun tmux/capture-pane ()
  (with-current-buffer (current-buffer)
    (when (equal major-mode 'tmux-session-mode)
      (setq-local buffer-read-only 'nil)
      (erase-buffer)
      (if tmux-session-name
	  (insert
	   (s-trim (tmux/command (format "capture-pane -t %s -pS -10000"
					 tmux-session-name))))
	(error "No session"))
      (setq-local buffer-read-only t))))

(defun tmux/command (cmd)
  (with-current-buffer (current-buffer)
    (shell-command-to-string
     (if (and (boundp 'ssh-addr)
	      (boundp 'ssh-hostname))
	 (progn
	   (format "ssh %s@%s \"tmux %s\""
		   ssh-hostname ssh-addr 
		   cmd))
       (format "tmux %s" cmd)))))

(defun tmux/refresh-all-buffers ()
  (dolist (b tmux-buffer-list)
    (with-current-buffer (get-buffer b)
      (tmux/capture-pane))))
