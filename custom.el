(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
     "4dcf06273c9f5f0e01cea95e5f71c3b6ee506f192d75ffda639d737964e2e14e"
     default))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(org-format-latex-header
   "\\documentclass{article}\12\\usepackage[usenames]{color}\12\\usepackage{bm}\12[DEFAULT-PACKAGES]\12[PACKAGES]\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}\12\\newcommand{\\f}[2]{\\frac{#1}{#2}}\12\\newcommand{\\bb}[1]{\\mathbb{#1}}\12\\newcommand{\\up}[2]{{#1}^{#2}}\12\\newcommand{\\fk}[1]{\\mathfrak{#1}}")
 '(org-format-latex-options
   '(:foreground default :background default :scale 0.6 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
		 :use-image-magick t))
 '(org-latex-packages-alist '(("" "tikz-cd" t nil) ("" "tikz" t nil)))
 '(org-preview-latex-default-process 'dvisvgm)
 '(package-selected-packages
   '(annalist avy capf-autosuggest company-box counsel doom-modeline
	      eldoc-box eshell-git-prompt eshell-syntax-highlighting
	      flycheck goto-chg iedit lsp-ui multiple-cursors
	      org-modern rainbow-delimiters tokyonight-themes vertico
	      vterm))
 '(package-vc-selected-packages
   '((tokyonight-themes :url
			"https://github.com/xuchengpeng/tokyonight-themes")
     (org-mode :url "https://code.tecosaur.net/tec/org-mode" :branch
	       "dev"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
