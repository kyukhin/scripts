;; IRC client.
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps t)

(defvar ercmode nil "ERC mode")

(defun erc-fn (switch)
  (message "IRC mode for emacs.")
  (setq ercmode 1)
  (setq erc-autojoin-channels-alist
	'(("freenode.net" "#maria" "#maria-dev")
	  ("oftc.net" "#gcc")))
  (erc :server "irc.freenode.net" :port 6667 :nick "kyukhin")
      (erc :server "irc.oftc.net" :port 6667 :nick "kyukhin"))

(add-to-list 'command-switch-alist '("-erc" . erc-fn))

;;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(ecb-directories-buffer-namejit-lock-stealth-time 1 t)
;; '(ecb-directories-show-node-info (quote (if-too-long . name)))
;; '(ecb-layout-name "left5")
;; '(ecb-options-version "2.32")
;; '(ecb-show-sources-in-directories-buffer (quote never))
;; '(ecb-truncate-long-names nil)
;; '(ecb-windows-width 0.2)
;; '(inhibit-startup-screen t))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(require 'buffer-move)

(load-theme 'solarized-light t)

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . lisp-mode))

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(global-set-key [(control m)] 'reindent-then-newline-and-indent)

;; my elisp directories
;; (defvar elisp-path '("/usr/local/share/emacs/site-lisp"))
;; (mapcar '(lambda(p) (add-to-list 'load-path p)) elisp-path)

;; Auto-complete
;(add-to-list 'load-path "~/.emacs.d/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(ac-config-default)

;; Magit
;; (add-to-list 'load-path "~/.emacs.d/magit-1.2.0")
;;(require 'magit)

;; mo-git-blame
(add-to-list 'load-path "~/emacs-els")
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:stipple nil :background "#000000" :foreground "#00ff00" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 250 :width normal :family "misc-fixed")))))

(fset  'yes-or-no-p 'y-or-n-p)

(setq keyboard-type 'xterm-color)

(defun tabs-on  () (setq indent-tabs-mode t))
(defun tabs-off () (setq indent-tabs-mode nil))

;; If file path contains sqlite - use sqlite specific indentation
;; use Linux style otherwise
(defun maybe-sqlite-style ()
  (if (and buffer-file-name
	   (string-match "sqlite" buffer-file-name))
    (progn (setq indent-tabs-mode nil)
	   (c-basic-offset 2))
    (c-set-style "Linux")))

;; (setq-default c-indent-level      8)
;; (setq-default c-basic-offset      8)
;; (setq-default tab-width		  8)
;; (add-hook 'c-mode-hook 'tabs-on)
;; Above lines replaced with Linux style
(add-hook 'c-mode-hook 'maybe-sqlite-style)

(setq lua-indent-level 4)
(add-hook 'lua-mode-hook 'tabs-off)

;; (setq c-continued-statement-offset 4)
;; (setq c-argdecl-indent             0)
;; (setq c-brace-offset              -4)
;; (setq c-continued-brace-offset    -4)
;; (setq c-brace-imaginary-offset     0)
;; (setq c-label-offset              -4)
;; (setq c-auto-newline             nil)
;; (setq comment-multi-line           t)

(setq make-backup-files nil)

(setq column-number-mode 1)

(normal-erase-is-backspace-mode 0)

(if (fboundp 'scroll-bar-mode      ) (scroll-bar-mode      -1))
(if (fboundp 'tool-bar-mode        ) (tool-bar-mode        -1))
(if (fboundp 'menu-bar-mode        ) (menu-bar-mode        -1))
(if (fboundp 'global-font-lock-mode) (global-font-lock-mode 1))

(setq scroll-preserve-screen-position 1 )
(setq scroll-step                     1 )
(setq scroll-conservatively           20)

(global-set-key "\C-s" 'isearch-forward-regexp )
(global-set-key "\C-r" 'isearch-backward-regexp)

;;; View tags other window
(defun view-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

(global-set-key [f1]  'revert-buffer)
(global-set-key [f2]  'buf-move-down)
(global-set-key [f3]  'whitespace-mode)
(global-set-key [f4]  'view-tag-other-window)
(global-set-key [f5]  'goto-line)
(global-set-key [f6]  'ispell-region)
(global-set-key [f7]  'ispell-buffer)
(global-set-key [f8]  'delete-trailing-whitespace)
(global-set-key [f9]  'comment-region)
(global-set-key [f10] 'uncomment-region)
(global-set-key [f11] 'search-word-under-cursor-forward)
(global-set-key [f12] 'search-word-under-cursor-backward)

(defun search-word-under-cursor-backward ()
  (interactive)
  (search-backward (current-word)))

(defun search-word-under-cursor-forward ()
  (interactive)
  (search-forward (current-word)))

(defun kyukhin-remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(put 'upcase-region 'disabled nil)

(defun kyukhin-print-to-pdf ()
  (interactive)
  (ps-spool-buffer)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf"))
  )

(setq ps-print-header           nil)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; Mail
;;(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
;;                                  (nnimap-address "imap.gmail.com")
;;                                  (nnimap-server-port 993)
;;                                  (nnimap-stream ssl)))
;(setq gnus-select-method '(nnimap "gmail"
;				  (nnimap-address "localhost")
;				  (nnimap-server-port 1984)
;				  (nnimap-stream ssl)))


;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "kirill.yukhin@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-local-domain "ims.intel.com")

;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; (defalias 'perl-mode 'cperl-mode)
;; (defvaralias 'cperl-indent-level 'tab-width)

(add-hook 'perl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq indent-tabs-mode nil)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
)

;; Hunspell

;; список используемых нами словарей
(setq ispell-local-dictionary-alist
      '(("russian"
	 "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
	 "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
	 "[-]"  nil ("-d" "ru_RU") nil utf-8)
	("english"
	 "[A-Za-z]" "[^A-Za-z]"
	 "[']"  nil ("-d" "en_US") nil iso-8859-1)))

;; вместо aspell использовать hunspell
(setq ispell-really-aspell nil
      ispell-really-hunspell t)

;; полный путь к нашему пропатченному hunspell
(setq ispell-program-name "/usr/bin/hunspell"
      ispell-dictionary "russian")

;; Команды емаксу в русской раскладке.
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
	(shell "*shell*")
	;; Don't ask about active buffers upon exit.
	(set-process-query-on-exit-flag (get-process "shell") nil)
	(rename-buffer "aux2")
	(shell "*shell*")
	(set-process-query-on-exit-flag (get-process "shell<1>") nil)
	(rename-buffer "aux")
	(add-hook 'comint-exec-hook
		(lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))
)

(if (not (bound-and-true-p ercmode))
	(funcall 'start-shells-fn))
	
;;(require 'auto-answer)
;;(let ((auto-answer '(("\\`Active processes exist; kill them and exit anyway\\? \\'" t))))
;;  (save-buffers-kill-emacs))

(split-window-vertically)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))

(setq split-height-threshold 1200)
(setq split-width-threshold 2000)
