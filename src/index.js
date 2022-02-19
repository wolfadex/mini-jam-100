import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  flags: {
    highScore: JSON.parse(localStorage.getItem("high-score")) || 0,
    initialSeed: Date.now(),
  },
});

app.ports.saveScore.subscribe(function (score) {
  localStorage.setItem("high-score", JSON.stringify(score));
});
