;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c i") #'consult-outline); outline-regexp: ";;;"; -*-

;;; STARTUP
(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Startup in %s sec with %d garbage collections"
                       (emacs-init-time "%.2f")
                       gcs-done)))

;;; PACKAGE
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; FONTS
(defun nm/get-default-font()
    "Consolas-13")
(add-to-list 'default-frame-alist `(font . ,(nm/get-default-font)))
(set-face-attribute 'default nil :height 110)

;;; THEMES
(load-theme 'modus-vivendi t)

;;; Don't add custom setting here
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file t)

;;; Interface tweaks
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode)
(show-paren-mode 1)
(tab-bar-mode -1)

(defun nm/paste-from-clipboard()
  (interactive)
  (setq x-select-enable-clipboard t)
  (yank)
  (setq x-select-enable-clipboard nil))

(defun nm/copy-to-clipboard()
  (interactive)
  (setq x-select-enable-clipboard t)
  (kill-ring-save (region-beginning) (region-end))
  (setq x-select-enable-clipboard nil))

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(set-selection-coding-system 'utf-8)
'(keyboard-coding-system 'utf-8)

(global-hl-line-mode t)
(setq backup-directory-alist `(("." . "~/.emacs.d/Trash")))

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)))

(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosave/" t)))

;; Set grep and find locations
(when (eq system-type 'windows-nt)
  (setq exec-path (add-to-list 'exec-path "C:\\\"Program Files\"\\Git\\usr\\bin"))
  (setq find-program "C:\\\"Program Files\"\\Git\\usr\\bin\\find.exe")
  (setq grep-program "C:\\\"Program Files\"\\Git\\usr\\bin\\grep.exe")
  (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
    "Use cygwin's /dev/null as the null-device."
    (let ((null-device "/dev/null"))
    ad-do-it))
    (ad-activate 'grep-compute-defaults))

;; Switch to grep buffer afeter invoked
(with-eval-after-load 'rgrep
  (advice-add 'rgrep :after
	      #'(lambda (_pattern _files _dir &optional _literal _confirm _flags) (pop-to-buffer (rg-buffer-name)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-with 4
              indent-tabs-mode nil
              compilation-scroll-output t
              visible-bell t
              initial-scratch-message nil
              use-dialog-box nil
              use-short-answers t
              fast-but-imprecise-scrolling t
              confirm-kill-processes nil
              native-comp-async-report-warnings-errors 'silent
              truncate-string-ellipsis "..."
              delete-by-moving-to-trash t
              scroll-step 8
              completions-detailed t
              next-error-message-highlight t
              read-minibuffer-restore-windows t
              history-lenght 25
              savehist-mode 1
              display-line-numbers-type 'relative
              )

;;; utilities

(defun nm/add--mode-hook-suffix (sym)
  "add -mode-hook to SYM and return the resulting symbol.
  If SYM already ends in -mode-hook, return it without appending anything"
  (let ((name (symbol-name sym))
        (suf "-mode-hook"))
    (if (cl-search suf name)
        sym
      (intern
       (concat name suf)))))

(defmacro nm/fix-evil-hook (hook state key fn)
  (let ((mode (nm/add--mode-hook-suffix hook)))
    `(add-hook ',mode
               (lambda ()
                 (evil-local-set-key ',state (kbd ,key)
                                     ',fn)))))

;;; project.el
(require 'project)
(global-set-key (kbd "M-O") 'project-find-file)
(global-set-key (kbd "M-s") 'rgrep)

;;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

;;; yassnippet
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dir '(~/.emacs.d/plugins/yasnippet))
  (yas-global-mode 1))

;;; evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-symbol-word-search t)
  :config
  (evil-mode t)
  (setq evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-tree)

  (nm/fix-evil-hook treemacs treemacs "H" evil-window-top)
  (nm/fix-evil-hook compilation motion "TAB" compilation-next-error)

  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "C-M-.") 'xref-find-definitions)
)

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init
   (list 'dired 'ibuffer 'mu4e)))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package vimish-fold
  :ensure t
  :after evil)

;;; Completion / Vertico

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        read-buffer-completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t)

(use-package which-key
  :ensure t)

;;; treemacs

(use-package treemacs
  :ensure t
  :defer t
  :hook (treemacs-mode . treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-recenter-after-project-jump 'always
        treemacs-no-delete-other-windows t
        treemacs-tag-follow-delay 0.1
        treemacs-recenter-after-file-follow 'always)
  (treemacs-project-follow-mode 1)
  (treemacs-follow-mode 1))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode . rainbow-delimiters-mode))
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "dark green"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "violet"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "coral"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "siennal"))))))

;;Eglot

(use-package eglot
  :ensure t
  :config
  ;; Configura eglot para usar clangd
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  :hook (c++-mode . eglot-ensure))

;;; COMPANY MODE

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode))
         (c++-mode . company-mode)
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 2)
   (setq company-backends '((company-capf)))) 

;;; clang-format

(use-package clang-format
  :ensure t)

(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") 'clang-format-region)
            ;; (add-hook 'before-save-hook 'clang-format-buffer nil 'local)
            ))

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") 'clang-format-region)
            ;; (add-hook 'before-save-hook 'clang-format-buffer nil 'local)
            ))
(defun clang-format-get-options ()
  "Parse the .clang-format file and return options as an alist."
  (let ((options '()))
    (with-temp-buffer
      ;; Leer el archivo .clang-format
      (insert-file-contents (expand-file-name ".clang-format" (project-root (project-current))))
      (goto-char (point-min))
      (while (re-search-forward "\\([a-zA-Z0-9_]+\\): \\([0-9]+\\|true\\|false\\|[^\n]+\\)" nil t)
        (let* ((key (intern (match-string 1)))
               (value (match-string 2)))
          (cond
           ((string-match-p "^[0-9]+$" value)
            (setq value (string-to-number value)))
           ((string= value "true") ;; Si es "true"
            (setq value t))
           ((string= value "false") ;; Si es "false"
            (setq value nil)))
          (setq options (cons (cons key value) options)))))
    options))

(defun set-clang-format-style-from-file ()
  "Set indentation style based on the .clang-format file in the project's root directory."
  (when (require 'project nil 'noerror) 
    (let ((project-root (project-current)))
      (when (and project-root
                 (file-exists-p (expand-file-name ".clang-format" (project-root project-root))))
        (setq clang-format-style "file")
        (message "Found .clang-format in project root: %s" (project-root project-root))
        (let ((clang-format-options (clang-format-get-options)))
          (let ((indent-width (cdr (assq 'IndentWidth clang-format-options)))
                (use-tab (cdr (assq 'UseTab clang-format-options)))
                (tab-width (cdr (assq 'TabWidth clang-format-options))))
            ;; Asegurarse de que los valores sean enteros
            (setq c-basic-offset (if (integerp indent-width) indent-width 4)) ;; Valor por defecto
            (setq indent-tabs-mode (if use-tab t nil)) ;; Usar tabulaciones o espacios
            (setq tab-width (if (integerp tab-width) tab-width 4)) ;; Valor por defecto
            (setq c++-tab-always-indent t)  ;; Indentar siempre con tabulaciones
            (message "Applied .clang-format settings: IndentWidth=%d, UseTab=%s, TabWidth=%d"
                     c-basic-offset
                     indent-tabs-mode
                     tab-width)))))))
(add-hook 'c++-mode-hook 'set-clang-format-style-from-file)
(add-hook 'c-mode-hook 'set-clang-format-style-from-file)
;;; WINUM

(use-package winum
  :ensure t
  :config
  (setq winum-scope 'frame-local)
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (global-set-key (kbd "M-9") 'winum-select-window-9)
  (winum-mode))

(winner-mode t)
;;; FLYCHECK

(use-package flycheck
  :ensure t)

;;; coding 

(require 'cc-mode)
(use-package lua-mode
  :ensure t
  :config
    (setq lua-indent-level 4)
    (setq lua-default-application "lua"))

(setq auto-mode-alist
      (append
       '(("\\.cpp$"     . c++-mode)
         ("\\.hpp$"     . c++-mode)
         ("\\.c$"       . c++-mode)
         ("\\.h$"       . c++-mode)
	 ("\\.inl$"     . c++-mode)
         ("\\.emacs$"   . emacs-lisp-mode)
	 ("\\.claradb$" . js-mode)
         ("\\.txt$"     . indented-text-mode)
	 ("\\.ts$"      . typescript-mode)
	 ("\\.rml$"     . poly-rml-mode)
	 ("\\.rcss$"    . css-mode)
	 ("\\.lua$"     . lua-mode))
       auto-mode-alist))

(add-to-list 'auto-mode-alist '(".*\\(prj\\|premake\\).*\\.lua$" . indented-text-mode))

(ido-mode)

;;; Keybindings

(global-set-key (kbd "C-<tab>") 'my-cycle-project-buffers)

(global-set-key (kbd "C-<tab>") 'bs-cycle-next)
(global-set-key (kbd "C-S-<tab>") 'bs-cycle-previous)
(global-set-key (kbd "C-l") 'ibuffer)

(global-set-key (kbd "C-q") 'kill-this-buffer)
