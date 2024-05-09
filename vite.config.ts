import { defineConfig } from "vite";
import { plugin as Elm } from "vite-plugin-elm";

export default defineConfig({
    plugins: [Elm()],
});