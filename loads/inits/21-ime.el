;;; 21-ime.el --- 日本語入力の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Windows の GUI は W32-IME、それ以外の OS は Mozc を使う。

;;; Code:

;;; tr-ime - Windows GUI の IME 連携
(use-package tr-ime
  :straight t
  :if (and IS-WINDOWS (eq window-system 'w32))
  :config
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize))

;;; mozc - Windows 以外の日本語入力
(use-package mozc
  :straight t
  :unless IS-WINDOWS
  :init
  (setq default-input-method "japanese-mozc"))

(provide '21-ime)
;;; 21-ime.el ends here
