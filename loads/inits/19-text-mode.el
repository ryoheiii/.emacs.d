;;; text-mode
;; テキスト編集用のモード
;; 2012-03-18

;; text-modeでバッファーを開いたときに行う設定
(add-hook
 'text-mode-hook
 (lambda ()
   ;; 自動で長過ぎる行を分割する
;   (auto-fill-mode 1)

   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)

   ;; インデント幅を2にする: 働いていない
   (setq tab-width 2)
   (setq c-basic-offset 2)
   ;; ;; 自動改行（auto-new-line）と
   ;; ;; 連続する空白の一括削除（hungry-delete）を
   ;; ;; 有効にする
   ;; (c-toggle-auto-hungry-state 1)
   ;; (c-toggle-hungry-state 1)

   ;; CamelCaseの語でも単語単位に分解して編集する
   ;; GtkWindow         => Gtk Window
   ;; EmacsFrameClass   => Emacs Frame Class
   ;; NSGraphicsContext => NS Graphics Context
   (subword-mode 1)))
