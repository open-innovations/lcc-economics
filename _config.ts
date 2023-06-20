import lume from "lume/mod.ts";

import basePath from "lume/plugins/base_path.ts";
import metas from "lume/plugins/metas.ts"

const site = lume({
  src: "./site",
  location: new URL("https://open-innovations.github.io/lcc-economics/"),
});

site.use(metas());

site.use(basePath());

export default site;
