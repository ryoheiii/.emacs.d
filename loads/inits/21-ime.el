;;; 21-ime.el --- 日本語入力の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; IME (Input Method Editor) の設定

;;; Code:

;;;;;; [Group] IME - 日本語入力の設定 ;;;;;
;;; Tr-ime - windows 用設定
(use-package tr-ime
  :straight t
  :if IS-WINDOWS
  :config
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize)
  )

;;; Mozc - mozc 設定
(use-package mozc
  :straight t
  :if (display-graphic-p)
  :unless IS-WINDOWS
  :init
  (setq default-input-method "japanese-mozc")
  )

(provide '21-ime)
;;; 21-ime.el ends here
