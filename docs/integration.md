<!-- -*- gfm -*- -->
# tty の実サービス検証

通常の `make test` は外部サービスの稼働や認証に依存させない。
ここでは実プロセスを使う任意の手動検証と、#17 の実測結果を記録する。
固定応答のテストだけでは確認できない通信・変換・転送が対象である。

## 準備

- 検証専用の HOME に本設定と lockfile に一致するパッケージを準備する。
  `HOME`、`XDG_CONFIG_HOME`、`XDG_CACHE_HOME` をその専用ディレクトリへ向ける。
  日常の `var/` や straight のビルドキャッシュを共有しない。
- 外部コマンドを検証用の PATH へ追加し、`emacs -nw` で起動する。
  `M-: (display-graphic-p)` が `nil` であることを確認する。
- clipboard は専用 Xvfb の `DISPLAY` を使い、外部の `xclip` にも同じ値を渡す。
  日常のクリップボードを上書きしない。
- C/C++ は使い捨てのソースだけを開く。Copilot は有効な認証が必要であり、
  検証用 HOME で `M-x copilot-login` を行うか、既存の認証を安全に引き継ぐ。
  資格情報、認証診断の全文、実プロジェクトの内容をログへ残さない。
- 終了後は検証用 Emacs とその HOME で起動したサービスを停止し、
  検証専用データ（認証の複製を含む）だけを削除する。

## Mozc と clipboard

1. `mozc_emacs_helper` と `mozc_server` を導入する。
   `M-x toggle-input-method` で `japanese-mozc` を有効にする。
   `nihongo`、空白、Return の順に入力し、「日本語」が確定することを確認する。
   再び `M-x toggle-input-method` で解除する。
   `C-\` は別のコマンドに割り当て済みなので使わない。
2. xclip を導入し、`M-: xclip-mode`、`M-: xclip-method` で
   有効状態と実際の転送方式を確認する（今回の測定は `xclip`）。
   `M-: (kill-new "Emacsから日本語")` の後、同じ DISPLAY の別端末で
   `xclip -selection clipboard -o` を実行して文字列を照合する。
3. 別端末で `printf '%s' '外部アプリからEmacs' | xclip -selection clipboard -i`
   を実行し、Emacs の空バッファで `C-y` により貼り付けて照合する。
   転送は非同期なので、即時に旧内容が見えた場合は最大5秒待つ。

## clangd・irony・代替補完

使い捨ての Git プロジェクトを作り、次を `main.c` と `main.cpp` に保存する。

```c
struct Example { int field; };
int target(void) { return 42; }
int main(void) { struct Example obj; target(); obj.fi }
```

1. clangd を PATH に置き、プロジェクト直下の `.clangd` に次を保存する。

   ```yaml
   CompileFlags:
     Add: [-Wall]
   ```

2. C と C++ をそれぞれ開き、自動接続後に `M-: (eglot-managed-p)` が真になることを
   確認する。`obj.fi` の直後で `M-x completion-at-point` を実行すると `field` を補完できる。
   呼び出し側の `target` 上で `M-x xref-find-definitions` を実行すると2行目へ移動する。
   この間 `irony-mode` は無効であることも確認する。
3. irony-server を導入する（[C/C++ 設定](cpp.md) の導入手順を参照）。
   別の一時ディレクトリに `.clangd` と `compile_commands.json` を置かず、
   同じ `main.cpp` を開く。`irony-mode` が自動で有効になり、Eglot を使わず
   `field` を補完できることを確認する。
4. clangd と irony-server が見つからない専用環境で Emacs を再起動する。
   PATH だけでなく `var/hist/irony/bin/` にもサーバーが無いことを確認する。
   C++ バッファで両モードが無効、`ggtags-mode` が有効であることを確認する。
   `int fallback_identifier;` と次行の `int x = fallback_i` を入力し、
   `M-x completion-at-point` で末尾を `fallback_identifier` へ補完できることを確認する。

## Copilot と無効化

1. Node.js と Copilot language server を検証環境へ導入する。
   Emacs からは `M-x copilot-install-server` でも導入できる。
   `my/copilot-enabled` が `auto` または `t` であることを確認する。
2. 認証済みの環境で `sample.py` に次の使い捨てサンプルを入力する。

   ```python
   # Return the sum of two integers.
   def add(a, b):

   ```

3. 最後のインデント直後で `C-c j c` を実行し、実サーバーからの提案を待つ。
   `C-c j j` で確定し、提案がバッファへ挿入されることを確認する。
   提案内容は非決定的なので、特定の文字列との一致は要求しない。
4. `M-x customize-variable` で `my/copilot-enabled` を `nil` にして保存し、
   Emacs を再起動する。サーバーの実行ファイルは残したまま Python バッファを開く。
   `(featurep 'copilot)`、`(featurep 'copilot-chat)`、
   `(bound-and-true-p copilot-mode)` がすべて `nil` であることを確認する。
   これは起動時の無効化の検証であり、通信断からの自動再接続を保証するものではない。

## #17 の測定結果（2026-09-13）

Ubuntu 24.04.5 / WSL2、Emacs 30.2（非native ビルド）の実 pty で実施した。
HOME・パッケージ・Xvfb は日常環境から分離し、実サービスの応答を使って確認した。

| 対象 | 実体 | 結果 |
|---|---|---|
| Mozc | helper / server 2.28.4715.102 | 有効化 →「日本語」の変換・確定 → 解除に成功 |
| clipboard | xclip 0.13、専用 Xvfb | 日本語文字列の双方向転送に成功 |
| clangd | 18.1.3 | C/C++ とも自動接続・メンバー補完・2行目の定義参照に成功 |
| irony | server 1.6.1 / libclang 18.1.3 | Eglot を使わない C++ バッファで実補完に成功 |
| 代替補完 | clangd / irony-server 利用不能 | ggtags 有効化と Cape のバッファ内単語補完に成功 |
| Copilot | language server 1.545.0 / Node.js 22.23.1 | 認証状態 OK、実提案の取得・確定・挿入に成功 |
| Copilot 無効化 | 実行ファイルを残し、起動前に `nil` | 再起動後の本体・Chat・モードが無効であることを確認 |

C/C++ の実サービス測定は文法未導入の cc-mode 系で行った。
tree-sitter の編集回帰は `make test-cpp-config`、日常利用の評価と全面切替判断は #5 で扱う。
今回の clipboard 測定は X11 の xclip 経路に限定し、Wayland や Windows の実転送は含まない。
Copilot Chat の実通信もこの測定には含まない。
