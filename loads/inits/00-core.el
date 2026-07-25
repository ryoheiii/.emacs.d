;;; 00-core.el --- 基本設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の基本設定

;;; Code:

;;;;;; [Group] Coding System - 文字コード設定 ;;;;;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(cond
 (IS-MAC
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (IS-WINDOWS
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos))))



;;;;;; [Group] File Settings - ファイル操作関連 ;;;;;;
;; backup と auto-save は var/backup/ へ隔離して有効化(保存先は early-init.el で設定済み)
(setq make-backup-files t
      version-control t        ; 番号付きバックアップ
      delete-old-versions t    ; 古い版は確認なしで削除
      kept-new-versions 5
      kept-old-versions 1
      backup-by-copying t      ; シンボリックリンク先の実体を保護
      auto-save-default t
      delete-auto-save-files t
      create-lockfiles nil)    ; .#lock は作業ディレクトリを汚すため無効を維持

;; save 時に mode-line を一瞬光らせる
(defvar my/mode-line-flash-timer nil
  "mode-line フラッシュの復元用タイマー。進行中は non-nil。")
(defun my/flash-mode-line-on-save ()
  "保存時に mode-line を一瞬光らせる。フラッシュ進行中の再入では元色を再取得しない。"
  (unless (timerp my/mode-line-flash-timer)
    (let ((orig (face-background 'mode-line)))
      (set-face-background 'mode-line "dark green")
      (setq my/mode-line-flash-timer
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (set-face-background 'mode-line orig)
                                   (setq my/mode-line-flash-timer nil)))))))
(add-hook 'after-save-hook #'my/flash-mode-line-on-save)

;; シェルスクリプト（shebangがあるファイル）の保存時に実行権を付与
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; 行末の空白を保存時に削除(markdown はハードブレイク用の末尾空白があるため除外)
(defun my/delete-trailing-whitespace-except-markdown ()
  "Markdown 系バッファ以外で行末の空白を削除する。"
  (unless (derived-mode-p 'markdown-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook #'my/delete-trailing-whitespace-except-markdown)

;;; 全フレームのフォーカスアウト時に全バッファを保存して GC
;; after-focus-change-function は短時間の状態揺れを伴い反復呼び出しされ得る上、
;; read-event 内など任意のコンテキストから呼ばれる (Emacs 30.1 の説明どおり)。
;; そのため idle timer でデバウンスし、安定後の状態で「全フレームがフォーカスを
;; 失った」遷移エッジのみ保存・GC を実行する
(defvar my/all-frames-unfocused-p nil
  "直前の判定時点で全フレームがフォーカスを失っていたなら non-nil.")
(defvar my/focus-change-timer nil
  "フォーカス変化のデバウンス用 idle timer.")
(defun my/handle-focus-change ()
  "安定後のフォーカス状態を確認し、全フレーム喪失への遷移時のみ保存・GC を行う."
  (setq my/focus-change-timer nil)
  (let ((unfocused (not (seq-some #'frame-focus-state (frame-list)))))
    (when (and unfocused (not my/all-frames-unfocused-p))
      (save-some-buffers "!")
      (garbage-collect))
    (setq my/all-frames-unfocused-p unfocused)))
(defun my/after-focus-change ()
  "フォーカス変化をデバウンスし、状態が安定してから my/handle-focus-change を実行する."
  (when (timerp my/focus-change-timer)
    (cancel-timer my/focus-change-timer))
  (setq my/focus-change-timer
        (run-with-idle-timer 0.2 nil #'my/handle-focus-change)))

(add-function :after after-focus-change-function #'my/after-focus-change)

;;;;;; [Group] Completion - 補完設定 ;;;;;;
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;;;; [Group] Functionality - 機能拡張 ;;;;;;
;; narrowing を有効化
(put 'narrow-to-region 'disabled nil)
;; `upcase-region` / `downcase-region` を有効化
(put 'upcase-region 'disabled nil)   ; C-x C-u -> upcase
(put 'downcase-region 'disabled nil) ; C-x C-l -> downcase
;; 現在の関数名をウィンドウ上部に表示
(which-function-mode 1)

;;;;;; [Group] Scrolling - スクロール設定 ;;;;;;
(setq scroll-conservatively 101
      scroll-margin 0
      comint-scroll-show-maximum-output t)

;;;;;; [Group] Garbage Collection - GC設定 ;;;;;;
;; early-init.el で most-positive-fixnum に設定した GC 閾値を起動完了後に復元
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.2)))
(add-to-list 'warning-suppress-types '(undo discard-info))

;;;;;; [Group] Misc - その他 ;;;;;;
(setq vc-follow-symlinks t
      inhibit-startup-message t
      flyspell-use-meta-tab nil
      native-comp-async-report-warnings-errors 'silent)

;;; パフォーマンス向上
;;; https://ayatakesi.github.io/lispref/25.2/html/Output-from-Processes.html
(setq read-process-output-max (* 1024 1024))
;;; protesilaos - https://protesilaos.com/emacs/dotemacs
(setq blink-matching-paren nil)         ; 閉じ括弧を入力しても点滅させない
;;; doomemacs - https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el
;; ファイル検索を2回行わないようにする
(setq auto-mode-case-fold nil)
;; 長い行の双方向スキャン
(setq bidi-inhibit-bpa t)
;; フォーカスされていないウィンドウのカーソルを削除
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
;; 高速なスクロール
(setq fast-but-imprecise-scrolling t)
;; ドメインにpingを送信しない
(setq ffap-machine-p-known 'reject)

;; `idle-update-delay' は Emacs 30.1 で obsolete のため後継変数を使用
(setq which-func-update-delay 1.0)
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
