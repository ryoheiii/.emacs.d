<script>
/**
 * Pandoc の --include-after-body に渡す HTML 断片（script タグも必要）。
 * 各コードブロックに「Copy」ボタンを付け、本文だけをコピーする。
 */
document.addEventListener("DOMContentLoaded", () => {
  // ページ内のすべての <pre><code>...</code></pre> を取得
  document.querySelectorAll("pre > code").forEach((codeBlock) => {
    // コピーボタンを作成
    const button = document.createElement("button");
    button.className = "copy-button";
    button.innerText = "Copy";

    button.addEventListener("click", () => {
      // ボタンのラベルを含めず、code 要素のテキストだけをコピーする。
      navigator.clipboard.writeText(codeBlock.innerText).then(() => {
        // コピーできたときだけ成功表示へ変え、1.5 秒後に戻す。
        button.innerText = "Copied!";
        setTimeout(() => { button.innerText = "Copy"; }, 1500);
      });
    });

    // 親の pre 要素を、CSS で指定したボタン位置の基準にする。
    const pre = codeBlock.parentNode;
    pre.style.position = "relative";
    // code の外側へ置き、コピー対象の本文と分ける。
    pre.appendChild(button);
  });
});
</script>
