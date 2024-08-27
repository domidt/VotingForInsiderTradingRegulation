# Voting for InsiderTrading Regulation

This repository provides relevant material related to the JBF <a href="https://doi.org/10.1016/j.jbankfin.2024.107295" target="_blank">paper</a>. In particular, it includes the instructions in German language, the software files (z-tree), the experimental data, and the analysis code. Using this material, please cite the paper.

 
The folder `1_instructions` includes (0) the presentation used in the sessions with the general session overview and the trader screen for the explanation of the market environment, the instructions in German used in the experiments, i.e., (1) instructions for the EET, (2) instructions for the market experiment (once for treatment FIXED and once for treatment FLUCT), and (3) the experimenter instructions separated by treatment and regime order.

The folder `2_software` includes the packages for experimental implementation in zTree (v3.6.7) separated by treatment and regime order. *.ztt* files include the software text. *.zdata* files store input data, e.g., `structure.zdata` sets the regime order, and `tagnums.zdata` sets trader tags that link traders’ actions for observers. `Questionnaire.ztq` is the post-experiment questionnaire and concludes the session.

The folder `3_data_zTree` (separated into/FIXED and /FLUCT) includes the original data records from zTree.

The folder `4_data_excel` stores the renamed *.xls* files of folder `3_data_zTree`. The files in both folders are linked via their file names.  We modified the files in `4_data_excel` such that the variable **Date** includes the **GroupID** and euro signs are replaced by 'Euro'.

The folder `5_analyses` provides the codes for (1) data preparation and (2) the analyses shown in the paper. Detailed variable descriptions are provided in `2_analyses_vote.pdf`.

The file `1_dataPreparation_vote.R` scripts the steps from loading files from the folder `4_data_excel` to the output of file `Data.RData`.

`Data.RData` consists of nine data tables: marketsummary, tradersummary, subjects, Votes, observers, transactions, offers, seconds, and avgtraderprofit.

The file `2_analyses_vote.Rmd` scripts the analyses in Rmarkdown format, i.e., calculations are in *.R* format and can be replicated in the *R* console. Moreover, this file compiles the `2_analyses_vote.pdf` as an output.