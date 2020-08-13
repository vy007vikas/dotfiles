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
         (lambda () (load-theme 'sanityinc-tomorrow-eighties t)))
(setq-default cursor-type 'hollow)
(setq-default indent-tabs-mode nil)
(use-package beacon
    :config
    (setq beacon-mode 1)
    (setq beacon-blink-delay 0.1)
    (setq beacon-blink-duration 0.3)
    (setq beacon-blink-when-point-moves 7)
    (setq beacon-color "blue"))

;; (rich-minority-mode 1)
;; (setq rm-blacklist
      ;; (format "^ \\(%s\\)$"
              ;; (mapconcat #'identity
                         ;; '("Fly.*" "Projectile.*" "Helm" "WK" "yas"
                           ;; "ivy" "company" "GitGutter" "Abbrev" "ElDoc")
                         ;; "\\|")))


;; ----------------------- GOOGLE Settings
(defvar vy/google3-home "/google/src/cloud/yadavvi/")
(require 'csearch)
(which-function-mode 1)
(global-set-key (kbd "C-c q") #'csearch)
(global-set-key (kbd "C-c g f") #'google3-format-region-or-buffer)
(global-set-key (kbd "C-c g t") #'google3-test)
(global-set-key (kbd "C-c g b") #'google3-build-cleaner)

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
      (setq name (vy/fig-client-prompt "Enter client name: "))
      (bookmark-set-filename bmk-record
                             (concatenate 'string vy/google3-home name suffix-path))
      (ad-set-arg 0 bmk-record))))

(defun vy/fig-client-prompt (prompt)
  "Prompts for all available citc clients."
  (completing-read prompt
                   (remove-if
                    (lambda (x) (string-match "^fig-export" x))
                    (directory-files vy/google3-home))
                   nil nil nil nil))

;; Toggle between java code and javatest file.
(defun vy/toggle-file-javatests ()
  (interactive)
  "Toggles between javatest and the java code for the file open right now."
  (if (string-match-p "/google3/javatests/" buffer-file-name)
      (let*
          ((file-path (replace-regexp-in-string "/google3/javatests/"
                                                "/google3/java/"
                                                buffer-file-name))
           (file-path (replace-regexp-in-string (regexp-quote "Test.java")
                                                ".java"
                                                file-path)))
        (find-file file-path))
    (let*
        ((file-path (replace-regexp-in-string "/google3/java/"
                                              "/google3/javatests/"
                                              buffer-file-name))
         (file-path (replace-regexp-in-string (regexp-quote ".java")
                                              "Test.java"
                                              file-path)))
      (find-file file-path))))
(global-set-key (kbd "C-c C-t") #'vy/toggle-file-javatests)


;; ----------------------- Personal customizations
;; Make emacs autocompletiopn case-sensitive
(setq dabbrev-case-fold-search nil)

;; Quickly comment line or region with C-c C-c
(defun vy/comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
(bind-key* "C-c C-c" #'vy/comment-or-uncomment-region-or-line)

;; Quickly close all buffers with C-x K
(defun vy/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))
(global-set-key (kbd "C-x K") #'vy/nuke-all-buffers)

(defun vy/things ()
  (interactive)
  (find-file "~/org/todo.org"))
(global-set-key (kbd "C-c t") #'vy/things)

;; Quickly reloading files
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-c C-r") #'revert-buffer-no-confirm)

;; Regex file search in directories
;; (defun vy/helm-grep-ag ()
;;   (interactive)
;;   (helm-grep-ag (read-directory-name "Search in: " default-directory nil t) nil))
;; (global-set-key (kbd "C-x C-r") #'vy/helm-grep-ap)


;; ----------------------- Optimized Desktop save mode
(defvar vy/desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar vy/desktop-session-name-hist nil
  "Desktop session name history")

(defun vy/desktop-save (&optional name)
  "Save desktop with a name."
  (interactive)
  (unless name
    (setq name (vy/desktop-get-session-name "Save session as: ")))
  (make-directory (concat vy/desktop-session-dir name) t)
  (desktop-save (concat vy/desktop-session-dir name) t))

(defun vy/desktop-read (&optional name)
  "Read desktop with a name."
  (interactive)
  (unless name
    (setq name (vy/desktop-get-session-name "Load session: ")))
  (desktop-read (concat vy/desktop-session-dir name)))

(defun vy/desktop-delete (&optional name)
  "Delete desktop with a name."
  (interactive)
  (unless name
    (setq name (vy/desktop-get-session-name "Delete session: ")))
  (delete-directory (concat vy/desktop-session-dir name) t))


(defun vy/desktop-get-session-name (prompt)
  (completing-read prompt (and (file-exists-p vy/desktop-session-dir)
                               (directory-files vy/desktop-session-dir))
                   nil nil nil vy/desktop-session-name-hist))

(global-set-key (kbd "C-c w w") #'vy/desktop-save)
(global-set-key (kbd "C-c w r") #'vy/desktop-read)
(global-set-key (kbd "C-c w d") #'vy/desktop-delete)


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


;; ----------------------- UI Customizations
;; Line highlighting
(global-hl-line-mode +1)
;; (set-face-attribute 'hl-line nil :foreground nil :background "gray10")

;; Column numbers
(column-number-mode t)

;; Parantheses
(show-paren-mode 1)

;; Remove toolbars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; ----------------------- Git gutter
(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (protobuf-mode . git-gutter-mode))
  :bind (("C-c <up>" . git-gutter:previous-hunk)
         ("C-c <down>" . git-gutter:next-hunk))
  :custom
  (git-gutter:handled-backends '(hg))
  :config
(custom-set-variables
 '(git-gutter:separator-sign "|")))
;; (set-face-foreground 'git-gutter:separator "#2d2d2d"))


;; ----------------------- Expand region
(require 'expand-region)
(global-set-key (kbd "C-o") 'er/expand-region)


;; ----------------------- Windmove
(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)


;; ----------------------- Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


;; ----------------------- Multiple Cursors
(require 'multiple-cursors)
;; MC-friendly packages.
(use-package phi-search :ensure t)
(use-package phi-rectangle :ensure t)
(use-package phi-search-mc :ensure t
  :config
  (phi-search-mc/setup-keys))
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c u") 'mc-hide-unmatched-lines-mode)

(defun vy/swiper-mc ()
  (interactive)
  (unless (require 'multiple-cursors nil t)
    (error "multiple-cursors isn't installed"))
  (let ((cands (nreverse ivy--old-cands)))
    (unless (string= ivy-text "")
      (ivy-set-action
       (lambda (_)
         (let (cand)
           (while (setq cand (pop cands))
             (swiper--action cand)
             (when cands
               (mc/create-fake-cursor-at-point))))
         (mc/maybe-multiple-cursors-mode)))
      (setq ivy-exit 'done)
      (exit-minibuffer))))
(global-set-key (kbd "C-c m") #'vy/swiper-mc)


;; ----------------------- Dimmer mode
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(setq dimmer-fraction 0.2)
(dimmer-mode t)


;; ----------------------- Org mode
(require 'org)
(setq org-agenda-files (list "~/org/work.org"))
(define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; ----------------------- Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-on-del-error-function #'ignore)
;; Always recentre when leaving Swiper
;; (setq swiper-action-recenter t)

(defun vy/swiper-with-region()
  (interactive)
  (if (region-active-p)
      (progn
        (setq querytext (buffer-substring (region-beginning) (region-end)))
        (deactivate-mark)
        (swiper querytext))
    (swiper)))
(global-set-key "\C-s" #'vy/swiper-with-region)

;; iMenu
(global-set-key (kbd "M-i") 'counsel-semantic-or-imenu)


;; ----------------------- Which-key mode
(require 'which-key)
(which-key-mode 1)


;; ----------------------- Ace Jump Mode
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; ;(setq ace-jump-mode-gray-background nil)
;; (setq ace-jump-mode-scope 'window)
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
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
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-find)

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
 '(avy-all-windows (quote all-frames))
 '(avy-background t)
 '(avy-timeout-seconds 0.3)
 '(compilation-error-regexp-alist
   (quote
    (google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback google-blaze-error google-blaze-warning google-log-error google-log-warning google-log-info google-log-fatal-message google-forge-python gunit-stack-trace absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line clang-include clang-include gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "99bc178c2856c32505c514ac04bf25022eaa02e2fddc5e7cdb40271bc708de39" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(git-gutter:handled-backends (quote (hg)))
 '(git-gutter:separator-sign "|")
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (rich-minority phi-search-mc phi-rectangle phi-search validate git-gutter beacon rainbow-delimiters multiple-cursors workgroups dimmer auto-highlight-symbol highlight-symbol buffer-move neotree zoom sublimity expand-region ivy swiper color-theme-sanityinc-tomorrow ample-theme diff-hl cyberpunk-theme ace-jump-mode meghanada lsp-java projectile spacemacs-theme afternoon-theme zenburn-theme yasnippet-snippets yaml-mode which-key undo-tree tabbar session rust-mode puppet-mode pod-mode muttrc-mode mutt-alias lsp-ui initsplit ido-completing-read+ htmlize graphviz-dot-mode goto-chg gitignore-mode gitconfig-mode gitattributes-mode git-modes folding ess eproject diminish csv-mode company-lsp browse-kill-ring boxquote bm bar-cursor apache-mode)))
 '(zoom-mode t nil (zoom)))


;;; .emacs ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
