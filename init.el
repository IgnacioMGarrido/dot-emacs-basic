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

;;; Don't add custom setting here
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file t)


;;; coding 

(require 'cc-mode)

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

;;; Interface tweaks
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode)
(show-paren-mode 1)

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

(load-theme 'tango-dark)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(set-selection-coding-system 'utf-8)
'(keyboard-coding-system 'utf-8)

(global-hl-line-mode t)
(setq backup-directory-alist            '((".*" . "~/.emacs/.Trash")))

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

;;; COMPANY MODE

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

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


