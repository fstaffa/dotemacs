(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(rainbow-mode
    evil
    evil-paredit
    surround
    color-theme-solarized
    cider
    rainbow-delimiters
    paredit
    clojure-mode
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

(load-theme 'solarized-dark t)
(global-rainbow-delimiters-mode)
(global-linum-mode 1)
;; no backup files
(setq make-backup-files nil)
;;no startup screen
(setq inhibit-startup-message t)

;;clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
