;;; 関数折りたたみ
(add-hook 'c++-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'c-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'scheme-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'python-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'enh-ruby-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(define-key global-map (kbd "C-\\") 'hs-toggle-hiding)

;;; 150721
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;;; 121209
(setq auto-mode-alist
      (append '(
                ("\\.go$"   . go-mode)
                ("\\.C$"    . c-mode)
                ("\\.cc$"   . cc-mode)
                ("\\.cpp$"  . c++-mode)
                ("\\.hh$"   . c++-mode)
                ("\\.c$"    . c-mode)
                ("\\.h$"    . c++-mode)
                ("\\.hpp$"    . c++-mode)
                ("\\.txt$"    . text-mode)
                ("\\.java$" . java-mode)
                ("\\.rb$" . enh-ruby-mode)
                ("\\.rake$" . enh-ruby-mode)
                ("\\.cap$" . enh-ruby-mode)
                ("Rakefile$" . enh-ruby-mode)
                ("config.ru$" . enh-ruby-mode)
                ("Gemfile$" . enh-ruby-mode)
                ("Capfile$" . enh-ruby-mode)
                ("\\.tex$"  . yatex-mode)
                ("\\.sty$"  . yatex-mode)
                ("\\.ltx$"  . yatex-mode)
                ("\\.cls$"  . yatex-mode)
                ("\\.clo$"  . yatex-mode)
                ("\\.clo$"  . yatex-mode)
                ("\\.bbl$"  . yatex-mode)
                ("\\.phtml$"     . web-mode)
                ("\\.tpl\\.php$" . web-mode)
                ("\\.jsp$"       . web-mode)
                ("\\.as[cp]x$"   . web-mode)
                ("\\.erb$"       . web-mode)
                ("\\.html?$"     . web-mode)
                ("\\.shtml$"     . web-mode)
                ("\\.xhtml$"     . web-mode)
                ("\\.js$"   . js2-mode)
                ("\\.el$"   . emacs-lisp-mode)
                )auto-mode-alist))
