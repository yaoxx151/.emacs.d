;;; init.el --- -*- lexical-binding: t -*-

(declare-function org-babel-tangle-file "ob-tangle")

;;System configs.
(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; Map Mac's command key to Meta only in GUI. Do it in iTerm2 config.
(if *sys/mac*
    (setq mac-command-modifier 'meta
	  mac-right-command-modifier 'super))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities
      '(("gnu" . 30)
	("nongnu" . 20)
	("melpa" . 10)))

;; Keep direct dependencies explicit so `package-autoremove' and clean-machine
;; setup do not depend on ignored Custom state.
(defconst my/package-manifest
  '(amx apheleia auctex avy beacon change-inner company counsel crux diff-hl
    dumb-jump exec-path-from-shell expand-region flycheck flyspell-correct-ivy
    goto-chg gptel highlight-indent-guides ibuffer-vc ivy magit marginalia
    markdown-mode multiple-cursors mwim rainbow-delimiters smartparens
    super-save swiper symbol-overlay undo-tree yank-media)
  "Packages configured directly by this Emacs setup.")
(setq package-selected-packages my/package-manifest)
(package-initialize)

;; Install use-package if not installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; Tangle only after the Org source changes.  Normal startup loads the generated
;; Lisp directly, so Org and Babel can remain deferred.
(let ((org-config (locate-user-emacs-file "myinit.org"))
      (el-config (locate-user-emacs-file "myinit.el")))
  (when (or (not (file-exists-p el-config))
            (file-newer-than-file-p org-config el-config))
    (require 'ob-tangle)
    (org-babel-tangle-file org-config el-config "emacs-lisp"))
  (load el-config nil 'nomessage))

;; `custom-file' is intentionally machine-local and may contain a stale value.
(setq package-selected-packages my/package-manifest)

;; Load work-related config.
(let ((work-config-file (locate-user-emacs-file "work.el")))
  (when (file-exists-p work-config-file)
    (load work-config-file)))
