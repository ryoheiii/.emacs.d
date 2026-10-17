# Issue #27: GUI の日本語操作案内

2026-10-18（JST）、隔離した Xvfb 上で日本語の操作案内を確認した。
6画面の日本語に欠字・重なり・意味が読めない切詰めはなく、ヘルプのリンクと戻る操作、
Git メニュー終了後の通常入力も成功した。製品設定の変更は不要だった。

## 環境

| 項目 | 実測・使用値 |
|---|---|
| ベース | `5cbc98b4de9d1362ee38bf8ee2ff2070abd01a2e`（日本語案内を統合済みの main） |
| 検査実装 | `08b114d6186f781e1f2e128e4c8c78c6ed224b03` |
| OS | Ubuntu 24.04.5 LTS、Linux x86_64 |
| Emacs | 31.1、GTK3 / X11 対応、`$HOME/.local/bin/emacs` |
| Xvfb | Ubuntu `2:21.1.12-1ubuntu1.8`、1440×1000、24 bit |
| フォント | default とヘルプの「日」の `font-at` はともに Ricty Diminished Discord、21 px |
| フレーム | 100列×35行、1146×866 px、基本文字セル11×23 px |
| 撮影 | Python 3、Pillow 10.2.0 の X11 ImageGrab |
| ShellCheck | CI と同じ0.11.0、配布物の SHA-256 も CI の固定値と照合 |

Xvfb と Pillow はダウンロードしたパッケージを `/tmp` 以下へ展開して使用した。
system package や日常の DISPLAY は変更していない。straight は専用コピーを使い、
コピー内の絶対 symlink を専用 `repos/` へ向け直し、元環境への参照がないことを確認した。
Makefile の既存 runner が作る一時 HOME・設定コピーと一時編集バッファだけを使用したため、
クリップボードへの fixture 書込みも日常の X server や編集中バッファには届かない。

## 実行と結果

GUI baseline は既存3件、拡張後は7件すべて成功した。startup baseline も成功した。
最終検証はすべて終了コード0、skip なしだった。

| コマンド | 結果 |
|---|---|
| `make test-startup` | init-loader エラー・未知の起動時警告なし |
| `make test-gui`（撮影あり） | 7/7 pass |
| `make test-gui`（撮影指定なし・一時的なフォント観測あり） | 7/7 pass |
| `make test-keybinding` | 26/26 pass |
| `make test-tty` | 3/3 pass |
| `make test-tty-live` | 33/33 pass |
| `make lint-el` | 成功 |
| `make test-guards` | 32/32 pass |

Emacs を使うコマンドには専用 `STRAIGHT_DIR` を渡した。実行時の構成は次のとおり。
`ISSUE27_TMP` は今回の一時環境であり、再実行時は新しく準備した場所へ置き換える。

```sh
ISSUE27_TMP=/tmp/emacs-issue-27-gui.Vxjw94
ISSUE27_STRAIGHT="$ISSUE27_TMP/straight"

make test-startup STRAIGHT_DIR="$ISSUE27_STRAIGHT"

PATH="$ISSUE27_TMP/root/usr/bin:$PATH" \
LD_LIBRARY_PATH="$ISSUE27_TMP/root/usr/lib/x86_64-linux-gnu" \
PYTHONPATH="$ISSUE27_TMP/root/usr/lib/python3/dist-packages" \
MY_TEST_GUI_CAPTURE_DIR="$ISSUE27_TMP/screenshots" \
xvfb-run -a -s '-screen 0 1440x1000x24 -nolisten tcp' \
make test-gui STRAIGHT_DIR="$ISSUE27_STRAIGHT"

make lint-el test-keybinding STRAIGHT_DIR="$ISSUE27_STRAIGHT"
env -u DISPLAY make test-tty test-tty-live STRAIGHT_DIR="$ISSUE27_STRAIGHT"
PATH="$ISSUE27_TMP/shellcheck/shellcheck-v0.11.0:$PATH" make test-guards
```

最初の `test-guards` は OS 同梱 ShellCheck 0.9.0 による既存シェルの SC2317 で2件失敗した。
文書・CI の指定どおり0.11.0へ揃えると32件成功した。シェル実装の変更や検査の無効化はしていない。
GUI 起動の stderr には隔離環境の DBus 接続に関する GLib 診断が出たが、
Emacs の起動検査・GUI の全ケースは成功した。

## 実画面と操作

次の6画像を実行担当とは別のエージェントが原寸で確認した。
画像は各1440×1000 px。実行時の一時証跡として上記 `screenshots/` に保存し、Git には追加していない。
恒久的な検証記録はこの表と再現手順である。

| 画像 | 確認した表示 | 判定 |
|---|---|---|
| `01-which-key.png` | `C-c h` の6項目 | 日本語を判読でき、欠字・重なりなし |
| `02-mx.png` | `save-buffer` 等の保存・終了の注釈 | キー・コマンド名・日本語の対応が明瞭 |
| `03-key-help.png` | `C-c h k C-x C-s` の日本語要約と原文見出し | 両方を判読可能 |
| `04-bindings.png` | `C-c h b` の保存キー・コマンドリンク・注釈 | 日本語を判読可能、リンクを維持 |
| `05-mode-help.png` | `C-c h m` の Org のタスク状態・タグ・予定日・移動 | 日本語を判読可能、リンクを維持 |
| `06-git-menu.png` | `C-x M-g` の Git 入口各列 | 列・見出しの重なりや読めない切詰めなし |

ヘルプは本文の関数リンクを `RET` で開き、ヘルプへフォーカスを戻して `l` を入力し、
元の履歴項目と日本語要約へ戻ることを検査した。`q` 後は元の編集バッファへ復帰した。
Git メニューは `C-g` で終了し、一時キーマップが消えた後に `a` が元バッファへ入力された。
いずれも `execute-kbd-macro` による command loop を通した。

ERT は idle timer 内で連続実行するため、ヘルプの `q` 後には普段の次の idle で走る
既存の `popwin:close-popup-window-if-necessary` を明示実行している。
メニューやリンクの操作自体は置き換えていない。

## 確認範囲

対象は上記環境・フレーム幅の日本語案内である。別 OS、別フォント、任意の極端に狭い画面は今回の結果に含めない。
既存の補助アイコンにはフォント欠字があるが、日本語案内の判読には影響せず、本 Issue の変更対象にしなかった。
`corfu-terminal`・`xclip` を含め製品設定は変更していない。

画像取得では組込み `x-export-frames` がこの環境で停止したため、動作を確認できた
Pillow ImageGrab を使った。撮影を指定しない通常の `test-gui` は Python/Pillow を要求しない。
