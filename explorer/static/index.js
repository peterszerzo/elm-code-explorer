const node = document.querySelector("#root");

const errorTemplate = `
<div class="code">
  <pre>
${window.elmError}
  </pre>
</div>
`;

if (window.Elm) {
  const app = Elm.Explorer.init({
    node
  });

  app.ports.scrollIntoView.subscribe(msg => {
    const node = document.querySelector(`#${msg}`);
    node.scrollIntoView({
      behavior: "smooth"
    });
  });
} else {
  node.innerHTML = errorTemplate;
}
