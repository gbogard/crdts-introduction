const Main = require("./output/Main");
require("./assets/style.scss");

function main() {
  document.body.innerHTML = "";
  Main.main();
}

if (module.hot) {
  module.hot.accept(function () {
    main();
  });
}

console.log("Starting app");
main();
