import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  flags: JSON.parse(localStorage.getItem("high-score")) || 0,
});

app.ports.saveScore.subscribe(function (score) {
  localStorage.setItem("high-score", JSON.stringify(score));
});
