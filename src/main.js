import { Elm } from "./Main.elm";

let API_BASE;

if (import.meta.env.PROD) {
  API_BASE = "https://policonnectapi.clubkokoa.com"
} else {
  API_BASE = "http://localhost:3000"
}

const app = Elm.Main.init({
  node: document.querySelector("#app"),
  flags: {
    apiBase: API_BASE,
  },
});