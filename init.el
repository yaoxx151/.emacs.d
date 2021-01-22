;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
;;   (setq mac-command-modifier 'meta) ; make cmd key do Meta
;;   (setq mac-option-modifier 'super) ; make opt key do Super
  ;; (setq mac-control-modifier 'meta) ; make Control key do Control
;;   (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
;;   (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

; backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-list-file-prefix "~/.saves/.saves-")
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 20
  kept-old-versions 10
  version-control t)

; highlight current word
(require 'highlight-symbol)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

; shell
(setq explicit-shell-file-name "/bin/bash")

; disable some keys
(global-unset-key (kbd "C-x C-b"))

; evil mode
;;(add-to-list 'load-path "~/.emacs.d/evil")
;; (require 'evil)
;;(evil-mode 1)
;; (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
;; (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)

; Hightlight current line.
; (global-hl-line-mode +1)
(require 'hl-line)
(add-hook 'prog-mode-hook 'hl-line-mode)
(set-face-background hl-line-face "#DCDCDC")

; Show line numbers by default
(global-linum-mode)
;; (add-hook 'prog-mode-hook 'linum-relative-mode)
;; (setq linum-relative-current-symbol "")

; tramp
(setq tramp-default-method "ssh")

; column width
(load "~/.emacs.d/elpa/column-marker.el")
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 81)))
(add-hook 'latex-mode-hook (lambda () (interactive) (column-marker-1 81)))

; Paren match.
(add-hook 'prog-mode-hook 'show-paren-mode)

; whitespace
(require 'whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'latex-mode-hook 'whitespace-mode)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-line-column nil)
(setq show-trailing-whitespace t)
(setq whitespace-style
              '(face
                tabs trailing space-before-tab space-after-tab
                ; tabs spaces trailing space-before-tab space-after-tab
        tab-mark))

; consider xx_xx in word
;; (add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

; automatically switch focus to newly splitted buffer
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

; Ivy
(require 'ivy)
(ivy-mode 1)

; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "M-z") 'undo-tree-undo)
(global-set-key (kbd "M-y") 'undo-tree-redo)

; Expand region.
(require 'expand-region)
(global-set-key (kbd "C-c C-y") 'er/expand-region)

; flycheck.
(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

; auto-complete-mode
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)
(remove-hook 'text-mode-hook 'company-mode)

; spell.
(global-set-key (kbd "C-c o") 'ispell-word)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(setq ispell-program-name "/usr/local/bin/ispell")

; crux
(require 'crux)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)

; comment
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

; helm.
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-c p") 'ignore)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action))

; Go to line.
(global-set-key (kbd "C-c g") 'goto-line)

; highlight indent.
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-foreground 'highlight-indent-guides-character-face "#C0C0C0")

; org mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (linum-mode -1)))
; make tab work in Org-mode code block
(setq org-src-tab-acts-natively t)
;; (add-hook 'latex-mode-hook 'visual-line-mode)
; (add-hook 'latex-mode-hook 'linum-relative-mode)

;; For python, import path
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PATH")

;; autopep8
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; isort
;; (require 'py-isort)
;; (add-hook 'before-save-hook 'py-isort-before-save)

; Handle copy and paste in OSX.
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

; some deletes don't need to kill
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

; bind them to emacs's default shortcut keys:
(global-set-key (kbd "M-<DEL>") 'my-backward-delete-word)

; ace jump.
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; projectile
;; (require 'projectile)
;; (projectile-mode 1)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (setq projectile-auto-discover nil)
;; (setq projectile-sort-order 'recentf)
;; (setq projectile-completion-system 'ivy)
;; (setq projectile-file-exists-remote-cache-expire nil)


; theme.
(if (display-graphic-p)
     ;; (disable-theme 'spacemacs-light)
    ;; (load-theme 'spacemacs-light t)
    (load-theme 'zenburn t)
(load-theme 'spacemacs-light t))

;; (if (display-graphic-p)
;;     (load "~/.emacs.d/org.el"))

;; Disable some useless stuffs.
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(setq-default visible-bell t)
(blink-cursor-mode 0)

;; prevent down-arrow from adding empty lines to the bottom of the buffer
;; (which is the default behaviour)
(setq next-line-add-newlines nil)

;;;;;;;;;;;;;; Added by program ;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f3455b91943e9664af7998cc2c458cfc17e674b6443891f519266e5b3c51799d" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "6e70d505e0957aaa67562ff0487b7b1b1e10f879655f2c47adf85949790fb687" default)))
 '(package-selected-packages
   (quote
    (ace-jump-mode evil-org py-isort py-autopep8 use-package zenburn-theme highlight-symbol linum-relative highlight-indent-guides crux markdown-mode+ magit elpy flycheck company-lsp lsp-ui lsp-mode expand-region multiple-cursors rainbow-delimiters undo-tree helm ivy fill-column-indicator))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#655370" :font "Lucida Grande" :height 1.2 :foreground "#2aa198" :bold))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#655370" :font "Lucida Grande" :height 1.2 :foreground "#5faf00" :bold))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#655370" :font "Lucida Grande" :height 1.2 :foreground "#268bd2" :bold))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#655370" :font "Lucida Grande" :height 1.2 :foreground "#800080" :bold))))
 '(variable-pitch ((t (:family "Helvetica Neue" :height 150 :weight normal)))))
