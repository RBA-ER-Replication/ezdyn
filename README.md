# ezdyn

`ezdyn` is an R package that aims to make policy analysis with (suites of) dynamic
economic models frictionless. Applying techniques from the ‘sufficient-statistics policy analysis’ literature, `ezdyn` can:
- complete analytical tasks (e.g. policy counterfactuals, optimal policy exercises, impulse response analysis, historical shock decompositions) across suites of models in a single line of code.
-  easily build and publish
interactive dashboards which allow such analysis to be completed code-free from your
web browser.

It works with models exported from Dynare or just a dataframe of impulse responses.

The
[whitepaper](https://github.com/RBA-ER-Replication/ezdyn/blob/main/ezdyn.pdf?raw=true)
articulates what the package can do, and full documentation (including a quickstart guide).

## Quickstart

Online
[live demo](https://01a1b082-f2eb-61b6-a308-c4c2c0261a0c.share.connect.posit.cloud/). For usage longer than 5-10 minutes, please install the package yourself (below).

To try ezdyn yourself:
1. Install R and RStudio
2. Install remotes package: `install.packages("remotes")`.
3. Install ezdyn: `remotes::install_github("RBA-ER-Replication/ezdyn")`.
2. Set your working directory to `dashboard_example/` and run
   `source("usa_dashboard.R")` to launch the example dashboard.

See the
[whitepaper](https://github.com/RBA-ER-Replication/ezdyn/blob/main/ezdyn.pdf?raw=true) for more detail.
