# resourcetransfer

This repository contains all code necessary to reproduce:

[*Gossip, reputation, domain-specificity, and resource transfers among Aka hunter-gatherers, Ngandu horticulturalists, and MTurkers*](https://grasshoppermouse.github.io/resourcetransfer/). Nicole H. Hess and Edward H. Hagen

Instructions:

0. Install [`quarto`](https://quarto.org/)
1. Clone this repository
2. Open the project in RStudio or `cd` into the directory and launch `R`. This will automatically bootstrap [`renv`](https://rstudio.github.io/renv/index.html).
3. After the bootstrapping process, enter the following in the console: `renv::restore()`. This should install all the necessary packages, including the separate data package [`resourcetransferdata2012`](https://github.com/grasshoppermouse/resourcetransferdata2012), in an isolated project-specific library.
4. Render the `paper.qmd` file using the RStudio GUI or with `quarto::quarto_render('paper.qmd')`. This will generate the preprint file [`paper.html`](https://grasshoppermouse.github.io/resourcetransfer/), which will display in the RStudio Viewer or can be viewed in any web browser. (Note: if not using RStudio, you will need a recent version of [`pandoc`](https://pandoc.org) installed.)

Note: Analyses used "R version 4.2.1 (2022-06-23)". You might need to install this version of R to reproduce them. Use [`rig`](https://github.com/r-lib/rig) to manage multiple versions of R.
