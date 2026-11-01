;;; .emacs --- kyukhin's Emacs config  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold
					       (* 16 1024 1024))))

;; What M-x customize saves goes to its own file, kept out of git
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
(setq grep-command "grep --color -nH -r -e ")

(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode 1)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)  ; built-in since Emacs 29
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Spelling, only where a spell checker is installed
(when (or (executable-find "aspell") (executable-find "hunspell") (executable-find "ispell"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; Show function in status
(which-function-mode 1)

(display-time-mode 1)

;;; IRC: `emacs -erc` connects over TLS to OFTC and joins #gcc
(use-package erc
  :defer t
  :defines (erc-autojoin-channels-alist erc-log-channels-directory erc-save-buffer-on-part)
  :functions erc-update-modules
  :config
  (setq erc-nick "kyukhin"
	erc-autojoin-channels-alist '(("oftc.net" "#gcc"))
	erc-log-channels-directory "~/.erc/logs/"
	erc-save-buffer-on-part t)
  (make-directory erc-log-channels-directory t)
  (add-to-list 'erc-modules 'log)
  (erc-update-modules))

(defun kyukhin-erc (_switch)
  "Connect to IRC; `emacs -erc` calls this."
  (erc-tls :server "irc.oftc.net" :port 6697 :nick "kyukhin"))
(add-to-list 'command-switch-alist '("-erc" . kyukhin-erc))

(defvar conf-shell nil "No shell mode")

(defun shell-fn (_switch)
  (message "Shells will start")
  (setq conf-shell t))

(add-to-list 'command-switch-alist '("-shell" . shell-fn))

(use-package solarized-theme :ensure t :config (load-theme 'solarized-light t))
(use-package magit :ensure t :bind ("<f11>" . magit-status))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(use-package markdown-mode :ensure t :mode "\\.md\\'")

(setq use-short-answers t)

(use-package lua-mode :ensure t :defer t
  :config (setq lua-indent-level 4)
  :hook (lua-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package go-mode :ensure t :defer t
  :hook (go-mode . (lambda ()
		     (add-hook 'before-save-hook #'gofmt-before-save nil t)
		     (setq tab-width 4
			   standard-indent 4
			   indent-tabs-mode t))))

(use-package company :ensure t :hook (prog-mode . company-mode))
(defun kyukhin-c-setup ()
  "C style of the Linux kernel, and clangd through eglot where it is installed."
  (c-set-style "linux")
  (when (executable-find "clangd") (eglot-ensure)))
(add-hook 'c-mode-hook #'kyukhin-c-setup)
(add-hook 'c++-mode-hook #'kyukhin-c-setup)

(setq make-backup-files nil)

(column-number-mode 1)

(normal-erase-is-backspace-mode 0)

(if (fboundp 'scroll-bar-mode      ) (scroll-bar-mode      -1))
(if (fboundp 'tool-bar-mode        ) (tool-bar-mode        -1))
(if (fboundp 'menu-bar-mode        ) (menu-bar-mode        -1))

(setq scroll-preserve-screen-position 1 )
(setq scroll-step                     1 )
(setq scroll-conservatively           20)

(global-set-key "\C-s" 'isearch-forward-regexp )
(global-set-key "\C-r" 'isearch-backward-regexp)

;;; F4: show the definition at point in the other window, keeping the point here
(defun kyukhin-peek-definition ()
  "Show the definition of the identifier at point in the other window."
  (interactive)
  (save-selected-window
    (call-interactively #'xref-find-definitions-other-window)
    (recenter 0)))

(global-set-key [f1]  'revert-buffer)
(global-set-key [f2] #'windmove-swap-states-down)
(global-set-key [f3]  'whitespace-mode)
(global-set-key [f4] #'kyukhin-peek-definition)
(global-set-key [f5]  'goto-line)
(global-set-key [f6]  'compile)
; (global-set-key [f6]  'ispell-region)
; (global-set-key [f7]  'ispell-buffer)
(global-set-key [f8]  'delete-trailing-whitespace)
(global-set-key [f9]  'comment-region)
(global-set-key [f10] 'uncomment-region)
(global-set-key [f12] 'search-word-under-cursor-forward)

; Swapping buffers
(windmove-swap-states-default-keybindings '(shift)) ; S-<arrow> swaps with the neighbour window

; Kill whole line w/ Alt+k
(global-set-key (kbd "M-k")  'kill-whole-line)

(defun search-word-under-cursor-backward ()
  (interactive)
  (search-backward (current-word)))

(defun search-word-under-cursor-forward ()
  (interactive)
  (search-forward (current-word)))

(defun kyukhin-remove-dos-eol ()
  "Hide the ^M characters of DOS line endings in the current buffer.
Useful in files that mix UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(put 'upcase-region 'disabled nil)

(with-eval-after-load 'diff-mode
  (set-face-foreground 'diff-added "green4")
  (set-face-foreground 'diff-removed "red3"))

;; Usage: emacs -diff file1 file2
(defun command-line-diff (_switch)
  "Compare the next two files of the command line with ediff."
  (setq inhibit-startup-screen t)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(add-hook 'perl-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; Команды емаксу в русской раскладке.
(defvar quail-keyboard-layout)
(declare-function quail-map "quail")
(declare-function quail-get-translation "quail")
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
	(modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
	(let* ((to (car map))
	       (from (quail-get-translation
		      (cadr map) (char-to-string to) 1)))
	  (when (and (characterp from) (characterp to))
	    (dolist (mod modifiers)
	      (define-key local-function-key-map
		(vector (append mod (list from)))
		(vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method "russian-computer")

(defun start-shells-fn ()
  "Starts shells if launched with -shell option."
  (if conf-shell
      (progn
	(split-window-vertically)
	(select-window (next-window (selected-window)))
	(shell "*shell*")
	;; Don't ask about active buffers upon exit.
	(set-process-query-on-exit-flag (get-process "shell") nil)
	(rename-buffer "bld")
	(shell "*shell*")
	(set-process-query-on-exit-flag (get-process "shell<1>") nil)
	(rename-buffer "aux")
	;;(switch-to-buffer-other-window "aux")
	(add-hook 'comint-exec-hook
		  (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))
	)
    )
)

(add-hook 'emacs-startup-hook 'start-shells-fn)

(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(provide '.emacs)
;;; .emacs ends here
