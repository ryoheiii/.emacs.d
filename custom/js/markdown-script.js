<script>
/**
 * - <pre><code>...</code></pre> ブロックに「Copy」ボタンを追加
 * - クリック時に中身をクリップボードにコピー
 */
document.addEventListener("DOMContentLoaded", () => {
  // ページ内のすべての <pre><code>...</code></pre> を取得
  document.querySelectorAll("pre > code").forEach((codeBlock) => {
    // コピーボタンを作成
    const button = document.createElement("button");
    button.className = "copy-button";
    button.innerText = "Copy";

    // ボタン押下時の処理
    button.addEventListener("click", () => {
      // コードブロック全体のテキストをそのままコピー
      navigator.clipboard.writeText(codeBlock.innerText).then(() => {
        // コピー成功時、ボタン表示を一時的に変更
        button.innerText = "Copied!";
        setTimeout(() => { button.innerText = "Copy"; }, 1500);
      });
    });

    // ボタンの絶対配置用に親 <pre> を relative に
    const pre = codeBlock.parentNode;
    pre.style.position = "relative";
    // <pre>内にボタンを追加
    pre.appendChild(button);
  });
});
</script>
