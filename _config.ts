import lume from "lume/mod.ts";

import basePath from "lume/plugins/base_path.ts";
import metas from "lume/plugins/metas.ts"

import oiCharts from "oi-lume-charts/mod.ts";

import autoDependency from "https://deno.land/x/oi_lume_utils@v0.3.0/processors/auto-dependency.ts";
import csvLoader from "https://deno.land/x/oi_lume_utils@v0.3.0/loaders/csv-loader.ts";

const site = lume({
  src: "./site",
  location: new URL("https://open-innovations.github.io/lcc-economics/"),
});

site.copy(['.css']);

site.loadData([".csv"], csvLoader);
site.process([".html"], autoDependency);

site.use(oiCharts({
  font: {
    // family: "chaparral-pro,sans-serif",
  },
}));

site.use(metas());

site.use(basePath());

export default site;
