import { Elm } from "./Main.elm";

function getRandomInt() {
  return Math.floor(Math.random() * 999999);
}

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: getRandomInt(),
});