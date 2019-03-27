(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

; (server-start)

; Don't use arrow key.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

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
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 20
  kept-old-versions 10
  version-control t)

; highlight current word
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

; shell
(setq explicit-shell-file-name "/bin/bash")

; disable some keys
(global-unset-key (kbd "C-x C-b"))

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

(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "M-<BACKSPACE>") 'my-backward-delete-word)
(global-set-key (kbd "M-<DEL>") 'my-backward-delete-word)

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
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

; automatically switch focus to newly splitted buffer
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

; Ivy
(require 'ivy)
(ivy-mode 1)

; Undo tree
(global-undo-tree-mode)

; rainbow delimieter
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Multiple cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-a") 'mc/edit-lines)

; Expand region.
(require 'expand-region)
(global-set-key (kbd "C-c C-y") 'er/expand-region)

; flycheck.
(add-hook 'prog-mode-hook 'flycheck-mode)

; auto-complete-mode
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
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)

; helm.
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-c p") 'ignore)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action))

; Mark the whole line default.
(defun select-current-line ()
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-c m") 'select-current-line)

; Go to line.
(global-set-key (kbd "C-c g") 'goto-line)

; Better undo.
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "M-/") 'undo-tree-redo)

; highlight indent.
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(require 'highlight-indent-guides)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-foreground 'highlight-indent-guides-character-face "#C0C0C0")

; org mode
(add-hook 'org-mode-hook 'visual-line-mode)
(remove-hook 'org-mode-hook 'linum-relative-mode)
(add-hook 'latex-mode-hook 'linum-relative-mode)

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

;;;;;;;;;;;;;; Added by program ;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6e70d505e0957aaa67562ff0487b7b1b1e10f879655f2c47adf85949790fb687" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme highlight-symbol linum-relative highlight-indent-guides crux markdown-mode+ magit elpy flycheck company-lsp lsp-ui lsp-mode expand-region multiple-cursors rainbow-delimiters undo-tree helm ivy fill-column-indicator))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; theme.
;; (load-theme 'spacemacs-light t)
(if (display-graphic-p)
    (load-theme 'zenburn)
  (load-theme 'spacemacs-light t))
