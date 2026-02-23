;;; 00-core.el --- 基本設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の基本設定

;;; Code:

;;;;;; [Group] Coding System - 文字コード設定 ;;;;;;
(set-language-environment "Japanese")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(cond
 (IS-MAC
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (IS-WINDOWS
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos))))



;;;;;; [Group] File Settings - ファイル操作関連 ;;;;;;
;; 自動保存・バックアップ設定
(setq backup-inhibited t
      delete-auto-save-files t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; save時にmode-line を一瞬光らせる
(add-hook 'after-save-hook
          (lambda ()
            (let ((orig-fg (face-background 'mode-line)))
              (set-face-background 'mode-line "dark green")
              (run-with-idle-timer 0.1 nil
                                   (lambda (fg) (set-face-background 'mode-line fg))
                                   orig-fg))))

;; シェルスクリプト（shebangがあるファイル）の保存時に実行権を付与
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; 行末の空白を削除
(defun my-toggle-delete-trailing-whitespace ()
  "Markdown-mode のときのみ `delete-trailing-whitespace` を無効にする."
  (if (derived-mode-p 'markdown-mode)
      (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
(add-hook 'markdown-mode-hook 'my-toggle-delete-trailing-whitespace)
(add-hook 'after-change-major-mode-hook 'my-toggle-delete-trailing-whitespace)

;;; フォーカスアウト時に全バッファを保存
(defun my/save-all-buffers ()
  (save-some-buffers "!"))

(add-hook 'focus-out-hook #'my/save-all-buffers)

;;;;;; [Group] Completion - 補完設定 ;;;;;;
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;;;; [Group] Clipboard - クリップボード設定 ;;;;;;
(setq x-select-enable-clipboard t) ;; クリップボードをシステムと共有

;;;;;; [Group] Functionality - 機能拡張 ;;;;;;
;; narrowing 禁止
(put 'narrow-to-region 'disabled nil)
;; `upcase-region` / `downcase-region` を有効化
(put 'upcase-region 'disabled nil)   ; C-x C-u -> upcase
(put 'downcase-region 'disabled nil) ; C-x C-l -> downcase
;; 現在の関数名をウィンドウ上部に表示
(which-function-mode 1)

;;;;;; [Group] Scrolling - スクロール設定 ;;;;;;
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1
      comint-scroll-show-maximum-output t)

;;;;;; [Group] Garbage Collection - GC設定 ;;;;;;
;; early-init.el で most-positive-fixnum に設定した GC 閾値を起動完了後に復元
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.2)))
(add-hook 'focus-out-hook #'garbage-collect)
(add-to-list 'warning-suppress-types '(undo discard-info))

;;;;;; [Group] Misc - その他 ;;;;;;
(setq vc-follow-symlinks t
      inhibit-startup-message t
      flyspell-use-meta-tab nil
      native-comp-async-report-warnings-errors 'silent)

;;; パフォーマンス向上
;;; https://ayatakesi.github.io/lispref/25.2/html/Output-from-Processes.html
(setq process-adaptive-read-buffering t)
;;; protesilaos - https://protesilaos.com/emacs/dotemacs
(setq blink-matching-paren nil)         ; 閉じ括弧を入力しても点滅させない
;;; doomemacs - https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el
;; ファイル検索を2回行わないようにする
(setq auto-mode-case-fold nil)
;; 双方向の並び替えを抑制する
(setq-default bidi-display-reordering 'left-to-right)
;; 長い行の双方向スキャン
(setq bidi-inhibit-bpa t)
;; フォーカスされていないウィンドウのカーソルを削除
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
;; 高速なスクロール
(setq fast-but-imprecise-scrolling t)
;; ドメインにpingを送信しない
(setq ffap-machine-p-known 'reject)

;; UIの更新頻度を下げる
(setq idle-update-delay 1.0)
;; 不要なフォント表示化を抑制
(setq redisplay-skip-fontification-on-input t)
;; Windowsの最適化
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))
;;; Centaur Emacs - https://github.com/seagle0128/.emacs.d
(when IS-WINDOWS
  (setq w32-use-native-image-API t))
(unless IS-MAC
  (setq command-line-ns-option-alist nil))

(provide '00-core)
;;; 00-core.el ends here
