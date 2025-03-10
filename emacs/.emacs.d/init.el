(defvar native-comp-deferred-compilation-deny-list nil)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-native-compile t)
(package-initialize)

(setq gc-cons-threshold (* 50 1000 1000))
(setq comp-async-report-warnings-errors nil)
(set-default-coding-systems 'utf-8)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when window-system
  (server-start))

(require 'init-packages)
(require 'init-ui)
(require 'init-completion)
(require 'init-utils)
(require 'init-tools)
(require 'init-org)
(require 'notebook)
;(require 'init-magit)

(require 'init-language-base)

(require 'init-keybindings)

(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
(defvar --backup-directory "~/MyEmacsBackups/")
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" "~/MyEmacsBackups/" t)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 3               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

; do not check if remote files are readable
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-exclude (quote ("Z:\\'")))

;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242730" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(copilot-chat-model "o1-preview")
 '(custom-safe-themes
   '("a68ec832444ed19b83703c829e60222c9cfad7186b7aea5fd794b79be54146e6" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "846b3dc12d774794861d81d7d2dcdb9645f82423565bfb4dad01204fa322dbd5" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "b494aae329f000b68aa16737ca1de482e239d44da9486e8d45800fd6fd636780" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "70cc30fd9d27a8d0d3ae82974ac2c409fd2cd5746470e2246778c6bec2d4857c" "ec8246f6f74bfe0230521412d88092342c17c1c0448a4b8ba39bddd3da170590" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "07e3a1323eb29844e0de052b05e21e03ae2f55695c11f5d68d61fb5fed722dd2" "d261bb8f66be37752791a67f03dd24361592ce141b32d83bcbe63ec1c738b087" "74a42b2b5dde1057e66bcf4c241789213e0ed5b77a2ee41c982fdc8c2abe9d98" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "72fda75af7caddec17ba9b49d2f99703c20a5f5f5c4dcec641d34a0b83569e88" "88a3c267ce2132defd46f2a4761925983dcbc35b1c3cfff1dded164ce169fed4" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "dd854be6626a4243375fd290fec71ed4befe90f1186eb5b485a9266011e15b29" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "f30aded97e67a487d30f38a1ac48eddb49fdb06ac01ebeaff39439997cbdd869" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "fa11f855b5f606f84e50106a7360c72aac88fee5f6fb8084aa4329009b61c5a2" "c3d385d9214f5b613e85fcaa2f746e52d272dbcceaaeea480ad3244a815882e7" "49de25b465bc3c2498bcd4c1575fa0090bd56fc79cdb49b919b49eaea17ee1dd" "ac88f5baa9b100652e0cc1d5891e66283d43a2b263cef048c3bff102983c29df" "86f4407f65d848ccdbbbf7384de75ba320d26ccecd719d50239f2c36bec18628" "a99e7c91236b2aba4cd374080c73f390c55173c5a1b4ac662eeb3172b60a9814" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default))
 '(fci-rule-color "#3C3D37")
 '(flymake-proc-allowed-file-name-masks
   '(("\\.xml\\'" flymake-xml-init nil nil)
     ("\\.html?\\'" flymake-xml-init nil nil)
     ("\\.cs\\'" csharp-flymake-init csharp-flymake-cleanup nil)
     ("\\.p[ml]\\'" flymake-perl-init nil nil)
     ("\\.php[345]?\\'" flymake-php-init nil nil)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup nil)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup nil)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup nil)
     ("\\.tex\\'" flymake-simple-tex-init nil nil)
     ("\\.idl\\'" flymake-simple-make-init nil nil)))
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(hl-highlight-mode t)
 '(irony-additional-clang-options '("-G" "Visual Studio 14 2015 Win64"))
 '(irony-extra-cmake-args '("-G" "Visual Studio 14 2015 Win64"))
 '(js3-auto-indent-p t)
 '(js3-bounce-indent-p t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-indent-dots t)
 '(js3-indent-level 4)
 '(js3-indent-on-enter-key t)
 '(js3-visitor-offset t t)
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(markdown-command "pandoc")
 '(org-agenda-files '("~/Sync/org/journal.org"))
 '(package-selected-packages
   '(msvc deadgrep spaceline-all-the-icons anzu spaceline spacemacs-theme cquery ccls dash lsp-ui lsp-mode rust-playground flycheck-rust rust-mode js2-mode zen-mode counsel-gtags counsel swiper ivy ripgrep prog-fill org-mime shell-pop git-messenger itail git-gutter origami zzz-to-char irony flycheck-clangcheck flycheck tide hl-anything less-css-mode web-mode typescript-mode gratuitous-dark-theme magit yaml-mode subatomic256-theme smex restclient rainbow-delimiters projectile powershell omnisharp multiple-cursors monokai-theme molokai-theme material-theme markdown-mode log4j-mode ido-vertical-mode highlight2clipboard groovy-mode gist ggtags fsharp-mode flx-ido flatland-theme expand-region csharp-mode company badwolf-theme))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t :background "#000")))
 '(org-block-begin-line ((nil :font "Consolas" :height 0.8 :slant italic)))
 '(org-level-1 ((t :inherit outline-1 :font "Cambria" :height 1.25)))
 '(org-level-2 ((t :inherit outline-2 :font "Cambria" :height 1.25)))
 '(org-level-3 ((t :inherit outline-3 :font "Cambria" :height 1.3)))
 '(org-level-4 ((t :inherit outline-5 :height 1.3)))
 '(org-level-5 ((t :inherit outline-5 :height 1.1))))

(provide 'init)

(setq native-comp-deferred-compilation t)
