;; Reduce the frequency of garbage collection by making it happen on
;; each 200MB of allocated data.
(setq gc-cons-threshold 200000000)

(require 'package)
(setq package-enable-at-startup nil)
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
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; Configs are in the org file.
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; Load work-related config.
(let ((work-config-file "~/.emacs.d/work.el"))
  (when (file-exists-p work-config-file)
    (load work-config-file)))
