;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c i") #'consult-outline); outline-regexp: ";;;"; -*-

;;; STARTUP
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Startup in %s sec with %d garbage collections"
                       (emacs-init-time "%.2f")
                       gcs-done)))

;;; PACKAGE
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; Unbind useless keybindings

(global-unset-key (kbd "C-x C-c")) ;;Unbind close emacs

;;; THEMES
;;(load-theme 'modus-vivendi t)
(setq custom-safe-themes t)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-city-lights))

;; For this to work on windows we might need to install nerd fonts
;; https://github.com/ryanoasis/nerd-fonts
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30))

(use-package all-the-icons
  :ensure t)

;;; FONTS
(defun nm/get-default-font()
    "Iosevka NF")
;;  "JetBrainsMono")
(add-to-list 'default-frame-alist `(font . ,(nm/get-default-font)))
(set-face-attribute 'default nil :height 110)

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

(global-auto-revert-mode 1)

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
              tab-width 4
              ;;indent-tabs-mode nil
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
;; use capslock as escape
(define-key key-translation-map (kbd "<capslock>") (kbd "<escape>"))

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

  (define-key evil-normal-state-map (kbd "M-g") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "C-M-g") 'xref-find-definitions)
)

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init
   (list 'dired 'ibuffer 'mu4e 'magit)))

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

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config (vertico-posframe-mode 1))

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

;; Set minibuffer on the top
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)

(defun my-vertico-buffer-list ()
  "Display a floating buffer list using Vertico."
  (interactive)
  (let ((buffers (mapcar 'buffer-name (buffer-list))))
    (when buffers
      (let ((selected (completing-read "Buffers: " buffers)))
        (switch-to-buffer selected)))))

(defun my-vertico-project-buffer-list ()
  "Display a floating buffer list using Vertico, showing only buffers from the current project, including the compilation buffer if it exists."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (buffers (cl-remove-if-not
                   (lambda (buffer)
                     (let ((buffer-file (buffer-file-name buffer)))
                       (and buffer-file
                            (string-prefix-p project-root buffer-file))))
                   (buffer-list)))
         (compilation-buffer (get-buffer "*compilation*")))
    (when compilation-buffer
      (push compilation-buffer buffers))
    (when buffers
      (let ((selected (completing-read "Project Buffers: " (mapcar 'buffer-name buffers))))
        (switch-to-buffer selected)))))

(global-set-key (kbd "C-c b") 'my-vertico-buffer-list)
(global-set-key (kbd "C-l") 'my-vertico-project-buffer-list)

;;; Treemacs
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

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode . rainbow-delimiters-mode)
         (c++-mode . rainbow-delimiters-mode))
  ;; :config
  ;; (custom-set-faces
  ;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
  ;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
  ;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "dark green"))))
  ;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
  ;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "violet"))))
  ;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "coral"))))
  ;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
  ;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "siennal")))))
  )

;;;Magit
;; Not sure if I like it using the command line looks enough
;; (use-package magit
;;   :ensure t)

;;;Eglot

(use-package eglot
  :ensure t
  :config
  ;; Configura eglot para usar clangd
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  :hook (c++-mode . eglot-ensure))

;;; Eldoc

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

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
           ((string= value "true")
            (setq value t))
           ((string= value "false")
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
            (setq c-basic-offset (if (integerp indent-width) indent-width 4))
            (setq indent-tabs-mode (if use-tab t nil))
            (setq tab-width 4);(if (integerp tab-width) tab-width 4))
            (setq c++-tab-always-indent 'complete)
            (message "Applied .clang-format settings: IndentWidth=%d, UseTab=%s, TabWidth=%d"
                     c-basic-offset
                     indent-tabs-mode
                     tab-width)))))))

;;; Project Compilation

(defun my-set-executable-path ()
  "Prompt for the executable path and update it in .dir-locals.el."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (dir-locals-file (concat root ".dir-locals.el"))
         (current-executable-path (if (boundp 'my-executable-path) my-executable-path "bin\\Release\\YourExecutable.exe"))
         (executable-path (read-file-name "Path to executable: " root current-executable-path))
         (dir-locals (if (file-exists-p dir-locals-file)
                         (with-temp-buffer
                           (insert-file-contents dir-locals-file)
                           (read (current-buffer)))
                       nil)))
    ;; Update or add `my-executable-path` in the locals
    (let ((new-locals (if (assoc 'nil dir-locals)
                          (append (assoc-delete-all 'my-executable-path (cdr (assoc 'nil dir-locals)))
                                  `((my-executable-path . ,executable-path)))
                        `((nil . ((my-executable-path . ,executable-path)))))))
      ;; Write the updated locals back to the file
      (with-temp-file dir-locals-file
        (insert (format "%S" new-locals))))
    (message "Executable path saved.")))

(defun my-load-executable-path ()
  "Load the saved executable path from .dir-locals.el."
  (let ((root (project-root (project-current t))))
    (unless (boundp 'my-executable-path)
      (hack-dir-local-variables))))

(defun my-launch-vs-debugger ()
  "Launch the Visual Studio debugger using the saved executable path."
  (my-load-executable-path)
  (let* ((root (project-root (project-current t)))
         (solution-file (car (directory-files root nil "\\.sln$"))))
    (when solution-file
      (start-process "vs-debugger" "*vs-debugger*"
                     "devenv.exe" (concat root solution-file) "/DebugExe" my-executable-path)
      (message "Starting debugging in Visual Studio..."))))

(defun my-compilation-finish (buffer msg)
  "Check if the build was successful, and if so, launch the Visual Studio debugger."
  (if (string-match "exited abnormally" msg)
      (message "Compilation failed.")
    (my-launch-vs-debugger)))

(defun my-save-last-compile-command ()
  "Saves the last compilation command in the .dir-locals.el of the project."
  (let* ((root (project-root (project-current t)))
         (dir-locals-file (concat root ".dir-locals.el"))
         ;; Read the existing .dir-locals.el if it exists
         (dir-locals (if (file-exists-p dir-locals-file)
                         (with-temp-buffer
                           (insert-file-contents dir-locals-file)
                           (read (current-buffer)))
                       nil)))
    (when root
      ;; Update or add `compile-command`
      (let ((new-locals (if (assoc 'nil dir-locals)
                            (append (assoc-delete-all 'compile-command (cdr (assoc 'nil dir-locals)))
                                    `((compile-command . ,compile-command)))
                          `((nil . ((compile-command . ,compile-command)))))))
        ;; Write the updated locals back to .dir-locals.el
        (with-temp-file dir-locals-file
          (insert (format "%S" new-locals))))
      (message "Compile command saved."))))
;; TODO Fix the hook to launch visual studio
(defun my-project-compile ()
  "Compile the project, save the last compile command, and launch the debugger if successful."
  (interactive)
  ;;(my-load-executable-path)
  (let ((default-directory (project-root (project-current t))))
    ;; Ask for executable path if not set
    ;; (unless (boundp 'my-executable-path)
    ;;   (my-set-executable-path))
    ;; ;; Set hook to save the compile command
    ;; (add-hook 'compilation-finish-functions 'my-compilation-finish)
    ;; Run the compile command
    (call-interactively 'compile)
    ;; Save the last compile command
    (my-save-last-compile-command)
	))

;;; Prg Hooks
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Set keybinding for opening Imenu
            (local-set-key (kbd "M-m") 'imenu)))
;;; C++ hooks
(add-hook 'c++-mode-hook 'set-clang-format-style-from-file)
(add-hook 'c-mode-hook 'set-clang-format-style-from-file)
(add-hook 'c++-mode-hook (lambda()
                        (setq c-set-style "k&r")
                        (setq electric-indent-mode -1)
                        (c-set-offset 'substatement-open 0)))
(add-hook 'c++-mode-hook (lambda()
			   (local-set-key (kbd "M-o") 'ff-find-other-file-other-window)
			   (local-set-key (kbd "<f5>") 'my-project-compile)
			   (local-set-key (kbd "<f3>") 'previous-error)
			   (local-set-key (kbd "<f4>") 'next-error)
			   ))

(defun run-my-executable (exe-path)
  "Run the executable specified by EXE-PATH."
  (interactive "fExecutable path: ")
  (start-process "my-executable" "*my-executable-buffer*" exe-path))

;;; AVY

(use-package avy
  :ensure t
  :bind
  (("M-g f" . avy-goto-char)
   ("M-j" . avy-goto-char-timer)
   ("M-g w" . avy-goto-word-1)
   ("M-g l" . avy-goto-line))
  :config
  (setq avy-background t))

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
  (winum-mode))

(winner-mode t)

;;; FLYCHECK

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-idle-change-delay 2)
  (setq flycheck-clang-args '("-std=c++17"))
  (setq flycheck-gcc-language-standard "c++17"))

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

;;; Visual Studio Integration
;; This will need some custom scripts.

(defface my-breakpoint-face
  '((t (:background "red")))
  "Face for marking breakpoints."
  :group 'my-custom-faces)

(defvar my-breakpoint-fringe-marker
  (propertize "*" 'display '(left-fringe right-triangle))
  "Marker for the breakpoint in the fringe.")

(defun my-set-breakpoint-overlay (line)
  "Set a breakpoint overlay at the given LINE."
  (let* ((overlay (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put overlay 'face 'my-breakpoint-face)
    overlay))

(defun my-set-breakpoint-fringe (line)
  "Add a fringe marker on the LINE."
  (let ((fringe-overlay (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put fringe-overlay 'before-string my-breakpoint-fringe-marker)
    fringe-overlay))

(defun my-set-breakpoint-current ()
  "Set a breakpoint at the current line by highlighting the line and placing a marker in the fringe."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (cmd (format "powershell -ExecutionPolicy Bypass -File ~/VSscripts/Set-Breakpoint.ps1 -filePath '%s' -lineNumber %d" file line)))
    (if (and file line)
        (progn
          ;; Set the breakpoint in Visual Studio
          (shell-command cmd)
          ;; Visual indication in Emacs
          (my-set-breakpoint-overlay line)
          (my-set-breakpoint-fringe line)
          (message "Breakpoint set at %s:%d" file line))
      (message "Not in a valid file buffer."))))

;; Optional: Define a key binding for setting breakpoints
(define-key prog-mode-map (kbd "C-c b") 'my-set-breakpoint-current)

;;; Keybindings

(global-set-key (kbd "C-<tab>") 'my-cycle-project-buffers)

(global-set-key (kbd "C-<tab>") 'bs-cycle-next)
(global-set-key (kbd "C-S-<tab>") 'bs-cycle-previous)
(global-set-key (kbd "C-c l") 'ibuffer)

(global-set-key (kbd "C-q") 'kill-this-buffer)

