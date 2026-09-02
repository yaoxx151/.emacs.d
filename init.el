;;; init.el --- -*- lexical-binding: t -*-

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

;; Load work-related config.
(let ((work-config-file (locate-user-emacs-file "work.el")))
  (when (file-exists-p work-config-file)
    (load work-config-file)))
