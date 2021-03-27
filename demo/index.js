const Main = require("./output/Main");
require("./assets/style.scss");

function main() {
  Main.main();
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("Reloaded, running main again");
    main();
  });
}

console.log("Starting app");
main();
