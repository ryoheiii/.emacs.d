(use-package rainbow-delimiters
  :ensure t
  :diminish
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  :bind (
         ("C-c l" . rainbow-delimiters-using-stronger-colors)
         )
  :config
  ;; these setting should be placed after load-theme
  ;; 括弧の色を強調
  (use-package cl-lib :ensure t)
  (use-package color  :ensure t)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 50))))

  ;; making unmatched parens stand out more
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground     'unspecified
                      :inherit        'error
                      :strike-through t)
  )
