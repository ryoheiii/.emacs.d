(use-package e2wm
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c +") 'e2wm:start-management)

    (setq e2wm:c-code-recipe
          '(| (:left-max-size 30)
              history
              (- (:upper-size-ratio 0.8)
                 main sub)))

    (e2wm:add-keymap
     e2wm:pst-minor-mode-keymap
     '(
       ("<M-left>" . e2wm:dp-code) ; codeへ変更
       ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
       ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
       ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
       ("C-c C-f"       . e2wm:pst-history-forward-command) ; 履歴を進む
       ("C-c C-b"       . e2wm:pst-history-back-command) ; 履歴をもどる
       ([f5]            . e2wm:dp-code-main-maximize-toggle-command)
                                        ;   ("prefix L"  . ielm)
                                        ;   ("M-m"       . e2wm:pst-window-select-main-command)
       ) e2wm:prefix-key)

    ;; (e2wm:start-management)
    )
  )
