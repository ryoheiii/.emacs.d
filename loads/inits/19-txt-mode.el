(add-hook
 'txt-mode-common-hook
 (lambda ()
   ;; ;; BSDスタイルをベースにする
   ;;    (c-set-style "bsd")

   (setq c-indent-level 2)

   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)

   ;; インデント幅を2にする
   (setq tab-width 2)
   (setq c-basic-offset 2)

   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)
   (c-toggle-hungry-state 1)
   (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

   (subword-mode 1)))
