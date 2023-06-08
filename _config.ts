import lume from "lume/mod.ts";
import basePath from "lume/plugins/base_path.ts";

const site = lume({
  src: "./site",
  location: new URL("https://open-innovations.github.io/lcc-economics/"),
});

site.use(basePath());

export default site;
