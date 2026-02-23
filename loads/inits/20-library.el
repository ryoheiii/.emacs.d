;;; 20-library.el --- 基盤ライブラリ -*- lexical-binding: t; -*-
;;; Commentary:
;; 他のパッケージが依存する基盤ライブラリの設定

;;; Code:

;;;; [Group] Library - ライブラリ関連 ;;;;;;
;;; dash - Emacs 用のモダンなリスト操作ライブラリ
(use-package dash
  :straight t
  :defer t)

;;; s - 文字列操作のための便利なユーティリティ
(use-package s
  :straight t
  :defer t)

;;; diminish - モードラインの表示を最適化
(use-package diminish
  :straight t
  :defer t)

(provide '20-library)
;;; 20-library.el ends here
