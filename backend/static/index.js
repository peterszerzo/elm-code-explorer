const node = document.querySelector("#root");

const errorTemplate = `
<div class="code">
  <pre>
${window.elmError}
  </pre>
</div>
`;

if (window.Elm) {
  Elm.Dashboard.init({
    node
  });
} else {
  node.innerHTML = errorTemplate;
}
