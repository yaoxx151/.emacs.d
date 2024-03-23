(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq gc-cons-threshold 100000000) ; https://github.com/MatthewZMD/.emacs.d?tab=readme-ov-file#defer-garbage-collection
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#f07171" "#86b300" "#f2ae49" "#399ee6" "#a37acc" "#55b4d4" "#575f66"])
 '(custom-safe-themes
   '("d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" default))
 '(elpy-rpc-backend "jedi" t)
 '(exwm-floating-border-color "#fcfcfc")
 '(fci-rule-color "#ede3e5")
 '(highlight-tail-colors
   ((("#eef2e1" "green" "green")
     . 0)
    (("#e9f3f6" "cyan" "blue")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#d9c2c6" "#ff9940"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#d9c2c6" "#86b300"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#d9c2c6" "#abb0b6"))
 '(objed-cursor-color "#f07171")
 '(package-selected-packages
   '(yasnippet-snippets beacon smart-hungry-delete zenburn-theme zen-and-art-theme xterm-color ws-butler winum white-sand-theme which-key volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacemacs-theme spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme reveal-in-osx-finder restart-emacs request rebecca-theme rainbow-delimiters railscasts-theme py-isort py-autopep8 purple-haze-theme projectile professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary organic-green-theme org-pomodoro org-mime org-download open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme neotree naquadah-theme mwim mustang-theme multiple-cursors multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum linum-relative link-hint light-soap-theme launchctl jedi jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide hungry-delete htmlize hl-todo highlight-symbol highlight-parentheses highlight-numbers highlight-indentation highlight-indent-guides heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy flyspell-correct flycheck-pos-tip flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump dracula-theme django-theme diminish diff-hl darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme crux counsel company-statistics column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cl-libify cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ac-ispell))
 '(pdf-view-midnight-colors (cons "#575f66" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#f07171" "#86b300" "#f2ae49" "#399ee6" "#a37acc" "#55b4d4" "#575f66"])
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#86b300")
    (cons 40 "#aab118")
    (cons 60 "#ceaf30")
    (cons 80 "#f2ae49")
    (cons 100 "#f4a345")
    (cons 120 "#f79841")
    (cons 140 "#fa8d3e")
    (cons 160 "#dd866d")
    (cons 180 "#c0809c")
    (cons 200 "#a37acc")
    (cons 220 "#bc77ad")
    (cons 240 "#d6738f")
    (cons 260 "#f07171")
    (cons 280 "#de8082")
    (cons 300 "#cd9093")
    (cons 320 "#bca0a4")
    (cons 340 "#ede3e5")
    (cons 360 "#ede3e5")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(put 'set-goal-column 'disabled nil)
