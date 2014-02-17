(load "~/.emacs.d/local/preinit.el")
(tool-bar-mode -1)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))


(require 'package)
(package-initialize)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") t )

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(evil
    magit
    dired+
    evil-paredit
    surround
    color-theme-solarized
    rainbow-delimiters
    projectile
    flx
    flx-ido
    paredit
    clojure-mode
    clojure-test-mode
    cider
    ac-nrepl
    auto-complete
    clojure-cheatsheet
    clj-refactor
    rvm)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))


;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; ido mode
(require 'flx-ido)
(ido-mode t)
(ido-everywhere)
(flx-ido-mode 1)

;; projectile
(projectile-global-mode)

;;auto-complete
(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(global-auto-complete-mode)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; cider
;;(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(require 'evil-paredit)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)

(require 'evil)
(evil-mode 1)
(require 'surround)
(global-surround-mode 1)
;; evil paredit support
;;(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
			       (clj-refactor-mode 1)))
(cljr-add-keybindings-with-prefix "C-c C-r")

(load-theme 'solarized-dark t)
(global-rainbow-delimiters-mode)
(global-linum-mode 1)
;; no backup files
(setq make-backup-files nil)
;;no startup screen
(setq inhibit-startup-message t)

;;clojure
 (add-hook 'clojure-mode-hook 
           (lambda () (interactive) (local-set-key (kbd "RET") 'newline-and-indent))) 



(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd ",>") 'paredit-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd ",d<") 'paredit-splice-sexp-killing-backward)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(defalias 'yes-or-no-p 'y-or-n-p)


;;minor
(setq calendar-week-start-day 1)
