(require 'e2wm)
(global-set-key (kbd "C-c +") 'e2wm:start-management)

;;; パースペクティブカスタマイズ

;;; code
;; キーバインド
(e2wm:add-keymap
 e2wm:pst-minor-mode-keymap
 '(
   ("<M-left>" . e2wm:dp-code) ; codeへ変更
   ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
   ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
   ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
;   ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
   ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
;   ("prefix L"  . ielm)
;   ("M-m"       . e2wm:pst-window-select-main-command)
   ) e2wm:prefix-key)

;(e2wm:add-keymap
; e2wm:dp-doc-minor-mode-map
; '(("prefix I" . info)) ; infoを起動する
; e2wm:prefix-key)
