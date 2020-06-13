;; .emacs --- Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; Welcome to Emacs (http://go/emacs).
;;
;; If you see this file, your homedir was just created on this workstation.
;; That means either you are new to Google (in that case, welcome!) or you
;; got yourself a faster machine.
;;
;; Either way, the main goal of this configuration is to help you be more
;; productive; if you have ideas, praise or complaints, direct them to
;; emacs-users@google.com (http://g/emacs-users).  We'd especially like to hear
;; from you if you can think of ways to make this configuration better for the
;; next Noogler.
;;
;; If you want to learn more about Emacs at Google, see http://go/emacs.

;;; Code:
;; Use the 'google' package by default.
(require 'google)


;; ----------------------- Melpa package list
(require 'cc-mode)
(require 'package)
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives 
    '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; ----------------------- Theme
(add-hook 'after-init-hook 
         (lambda () (load-theme 'cyberpunk t)))
;(load-theme 'ample t t)
;(enable-theme 'ample)
(setq-default cursor-type 'hollow)
(setq-default indent-tabs-mode nil)


;; ----------------------- GOOGLE Settings
;; csearch
(require 'csearch)
(global-set-key (kbd "C-c q") #'csearch)

;; lint
(global-set-key (kbd "C-c l") #'google-lint)
(require 'google3-build-cleaner)

;; Remove legacy p4 stuff for fig clients
(remove-hook 'find-file-hook 'p4-update-status)
(remove-hook 'find-file-hook 'google-load-p4-if-useful-hook) 
(setq vc-handled-backends nil)

;; User Cider LSP for code actions.
(require 'google3-eglot)
;(setq google3-eglot-enabled-modes (delq google3-eglot-enabled-modes 'python-mode))
(setq eglot-sync-connect 0)
(google3-eglot-setup)

;; Enable Google-specifc dynamic bookmarking for g4 clients
(defadvice bookmark-default-handler (before ash-google3-relativize activate)
  (when (is-google3-filename (bookmark-get-filename (ad-get-arg 0)))
    (let* ((bmk-record (copy-list (ad-get-arg 0)))
	   (file-path (bookmark-get-filename bmk-record))
	   (path-index (string-match-p "/google3/" file-path))
	   (suffix-path (substring file-path path-index (length file-path))))
      (bookmark-set-filename bmk-record (concatenate 'string "/google/src/cloud/yadavvi/" (read-string "Enter client-name:") suffix-path))
      (ad-set-arg 0 bmk-record))))

;; Quickly reloading files
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-c C-r") #'revert-buffer-no-confirm)


;; ----------------------- Personal customizations
;; Quickly comment line or region with C-/
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(bind-key* "C-c C-c" #'comment-or-uncomment-region-or-line)

(desktop-save-mode 1)


;; ----------------------- iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)  ; Open ibuffer
;; nearly all of this is the default layout
(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 40 40 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))

;; iMenu
(global-set-key (kbd "M-i") 'imenu)


;; ----------------------- UI Customizations
;; Line highlighting
(global-hl-line-mode +1)
(set-face-attribute 'hl-line nil :foreground nil :background "gray10")

;; Change Highlighting
(global-diff-hl-mode)

;; Column numbers
(column-number-mode t)

;; Parantheses
(show-paren-mode 1)

;; Remove toolbars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; ----------------------- Expand region
(require 'expand-region)
(global-set-key (kbd "C-o") 'er/expand-region)


;; ----------------------- Windmove
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


;; ----------------------- Workgroups
(require 'workgroups)
(workgroups-mode 1)
(setq wg-prefix-key (kbd "C-c w"))


;; ----------------------- Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


;; ----------------------- Dimmer mode
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(setq dimmer-fraction 0.3)
(dimmer-mode t)


;; ----------------------- Org mode
(require 'org)
(setq org-agenda-files (list "~/org/work.org"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; ----------------------- Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)


;; ----------------------- Which-key mode
(require 'which-key)
(which-key-mode 1)


;; ----------------------- Ace Jump Mode
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; ;(setq ace-jump-mode-gray-background nil)
;; (setq ace-jump-mode-scope 'window)
;; (global-set-key (kbd "M-o") 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; ;(setq aw-background nil)

;; ----------------------- Avy Jump Mode
(use-package avy
  :ensure t
  :custom
  (avy-all-windows 'all-frames)
  (avy-background t)
  (avy-timeout-seconds 0.3)
  :bind (("C-c SPC" . avy-goto-char-timer)))



;; ----------------------- company Mode 
;; Enable company-mode for all files
(add-hook 'after-init-hook #'global-company-mode)
;; (optional) Switch the shortcut for triggering autocompletion to F5.
(global-set-key (kbd "<f5>") #'company-complete)


;; ----------------------- Helm configuration
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)


;; ----------------------- Zoom mode
(setq zoom-minibuffer-preserve-layout t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error-regexp-alist
   (quote
    (google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback google-blaze-error google-blaze-warning google-log-error google-log-warning google-log-info google-log-fatal-message google-forge-python gunit-stack-trace absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line clang-include clang-include gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (workgroups dimmer auto-highlight-symbol highlight-symbol buffer-move neotree zoom sublimity expand-region ivy swiper color-theme-sanityinc-tomorrow ample-theme diff-hl cyberpunk-theme ace-jump-mode meghanada lsp-java projectile spacemacs-theme afternoon-theme zenburn-theme yasnippet-snippets yaml-mode which-key undo-tree tabbar session rust-mode puppet-mode pod-mode muttrc-mode mutt-alias lsp-ui initsplit ido-completing-read+ htmlize graphviz-dot-mode goto-chg gitignore-mode gitconfig-mode gitattributes-mode git-modes folding ess eproject diminish csv-mode company-lsp browse-kill-ring boxquote bm bar-cursor apache-mode)))
 '(zoom-mode t nil (zoom)))


;;; .emacs ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
