;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Package activation and site initialization happen before the regular init
;; file, so these settings must live here.
(setq package-enable-at-startup nil
      package-quickstart t
      load-prefer-newer t
      site-run-file nil)

;; Minimize collections during startup, then return to a moderate threshold.
(defconst my/normal-gc-cons-threshold (* 32 1024 1024))
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/normal-gc-cons-threshold
                  gc-cons-percentage 0.1)))

;;; early-init.el ends here
