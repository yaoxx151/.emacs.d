;; Performance improvement following https://github.com/MatthewZMD/.emacs.d?tab=readme-ov-file#defer-garbage-collection.
(setq gc-cons-threshold 100000000)

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
