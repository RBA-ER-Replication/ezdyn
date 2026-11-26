# ezdyn
`ezdyn` is an R package that aims to make policy analysis with (suites of) dynamic
economic models frictionless. Applying techniques from the ‘sufficient-statistics policy analysis’ literature, `ezdyn` can:
- complete analytical tasks (e.g. policy counterfactuals, optimal policy exercises, impulse response analysis, historical shock decompositions) across suites of models in a single line of code.
-  easily build and publish
interactive dashboards which allow such analysis to be completed code-free from your
web browser.

`ezdyn` works with models exported from Dynare or just a dataframe of impulse responses.

The
[whitepaper](https://github.com/RBA-ER-Replication/ezdyn/blob/main/ezdyn.pdf?raw=true)
articulates what the package can do, and full documentation (including a quickstart guide).

![The Optimal Policy tab. The baseline paths plotted are dummy data, making the results of the
exercise meaningless and illustrative.](tools/dashboard_screenshot.png)
*Figure 1: The Optimal Policy tab. The baseline paths plotted are dummy data, making the results of the
exercise meaningless and illustrative.*

## Quickstart

Online
[live demo](https://01a1b082-f2eb-61b6-a308-c4c2c0261a0c.share.connect.posit.cloud/). For usage longer than 5-10 minutes, please install the package yourself:
1. Install R and RStudio
2. Install remotes package: `install.packages("remotes")`.
3. Install ezdyn: `remotes::install_github("RBA-ER-Replication/ezdyn")`.
2. Set your working directory to `dashboard_example/` and run
   `source("usa_dashboard.R")` to launch the example dashboard.

See the
[whitepaper](https://github.com/RBA-ER-Replication/ezdyn/blob/main/ezdyn.pdf?raw=true) for more detail.
