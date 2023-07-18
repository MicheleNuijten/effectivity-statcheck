
# Meta-Data for the "Effectiveness of statcheck in Peer Review" Project
*Michèle B. Nuijten & Jelte M. Wicherts*

This GitHub/OSF project contains files for the project in which we compare the prevalence of statistical reporting inconsistencies between journals that implemented statcheck in their peer review process and matched control journals, before and after statcheck implementation.

## Important links
* The OSF project page: https://osf.io/q84jn/
* The GitHub project page: https://github.com/MicheleNuijten/effectivity-statcheck
* The preregistration: https://osf.io/umwea
* The preprint: https://psyarxiv.com/bxau9

## Author roles
Conceived of idea: Michèle Nuijten (MN), Jelte Wicherts (JW)  
Downloaded articles: Afra Kiliç (AK), Tsz Keung Wong (TKW), MN  
Wrote preregistration: MN, JW  
Scraped articles: MN  
Analyzed data: MN  
Wrote paper: MN
Edited text: JW

## Data origin
The data from this study are scraped from published psychology articles. Part of these articles were already downloaded for a previous project; the other part was downloaded for this project. All articles were scraped & checked with statcheck again for this project.

## Ethical approval
Ethical approval obtained from the Ethics Review Board of TSB on 18-11-2022. Approval number: TSB_RP773.

## Preregistration
This project was preregistered (a self-registration) on 30-11-2022. The preregistration, including power analysis, can be found here: [https://osf.io/umwea](https://osf.io/umwea).

## Data package history and access
During the first period of this project, all files were stored on a SURFdrive folder that MN and JW could access. Writing was done in shared Google docs that could be accessed by MN and JW.

Nearing publication of the preprint in January 2023, we openly shared all relevant files necessary to reproduce our research publicly on GitHub and OSF. 

For the full report (incl. exploratory analyses), we created a new version of statcheck that could deal with a specific type of spaces in JESP articles. We reran statcheck on all articles included in the sample and updated the data and analysis files accordingly. 

Reasons for small changes and updates are either listed below the main overview table or, from January 13 2023 onwards, listed automatically in the form of GitHub commits.

|What?|When?|Who?|	
|:--|:--|:--|
|Performed power analysis|Nov 2022|MN|
|Wrote preregistration|Oct/Nov 2022|MN, JW|
|Wrote downloading instructions for assistants|Oct 2022|MN|
|Downloaded existing sample of articles from our [GitHub page](https://github.com/chartgerink/sampleStatcheck); PS 2003-2013; JPSP 2003-2013; JEPG 2003-2013; shortened some file names to avoid error messages when scraping|Oct 2022|MN|
|Downloaded new sample of articles: PS 2013-2022; JEPG 2013-2022; JESP 2003-2022|Nov 2022|AK|
|Downloaded new sample of articles: JPSP (subsection ASC) 2013-2022|Nov/Dec 2022|TKW|
|Made list of all ASC titles of JPSP articles in existing sample|Nov/Dec 2022|TKW|
|Scraped articles with statcheck (incl. refinements to the code)|Dec 2022/Jan 2023|MN|
|Created new version of statcheck to deal with spacing in JESP|April-June 2023|MN|
|Scraped articles with new version of statcheck|June 2023|MN|
|Reran descriptive and confirmatory analyses & updated exploratory analyses|June-July 2023|MN|
|Wrote and edited full report|July 2023|MN, JW|

### Small changes and notes
* 2-12-2022: In JPEG, there were some articles (39 articles in total) without HTML files. So, Afra created a list of articles without HTML files: [https://catnip-ocelot-499.notion.site/Statcheck-io-the-articles-without-HTML-63712ffe996840dfb486b13a74bbed53](https://catnip-ocelot-499.notion.site/Statcheck-io-the-articles-without-HTML-63712ffe996840dfb486b13a74bbed53)
* 13-12-2022: MN realized that there was a mistake in the downloading instructions and JESP 2003-2013 weren’t downloaded yet. These remaining articles were downloaded by MN & AK between 13-12 and 19-12.
* 20-12-2022 to 23-12-2022: MN fixed issues with scraping dates from articles. JESP’s dates weren’t read by original regexes in code.
* 23-12-2022: when analyzing descriptives, MN realized that 28 articles from JEPG published in 2013 had no “date received” information, because they were just the abstracts, not the full files. Redownloaded these.
* 23-12-2022: MN realized that JPSP 2008 volume 1 was not downloaded. Downloaded & added to sample.
* 23-12-2022: in list of ASC titles of JPSP, retracted articles and editorials were still included. Removed those.
* 23-12-2022: reran scraping script on updated files.
* 02-01-2023: reran scraping script on updated files: discovered a bug in the list of ASC titles; some titles had special characters that weren’t read by R and as a result ~50 articles were unjustly excluded.
* 06-02-2023: found a bug in statcheck: it didn’t correctly check for correct rounding in negative test statistics. This required a bug fix in statcheck and a rerun of the scraping script & all analyses.

## File descriptions
The file descriptions below are organized in the same way as the components and folders on the OSF page at [https://osf.io/q84jn/](https://osf.io/q84jn/).

### 02 Preregistration
| File Name | Description | Created By |
|:--|:--|:--|
|221130preregistration_effectivitystatcheck.pdf|Preregistration of the project|MN, JW|

#### Power analysis
| File Name | Description | Created By |
|:--|:--|:--|
| 00extract_dates.R              | Helper function with regexes to extract dates                         | MN         |
| 00extract_pattern.R            | Helper function to extract a pattern from text                        | MN         |
| 00html_to_txt.R                | Helper function to convert html files to plain txt                    | MN         |
| 00scrape_html_dir.R            | Helper function that allows scraping a directory of html files        | MN         |
| 01scrape_articles.R            | Function to run statcheck & scrape additional meta-data from articles | MN         |
| 02data_wrangling.R             | Function to clean & reorder raw data                                  | MN         |
| 2022-11-23scraped_articles.txt | Raw, scraped data from articles                                       | MN         |
| 2022-11-28data_per_article.txt | Data organized at article level                                       | MN         |
| 2022-11-28data_wrangled.txt    | Data with additional variables and missing values removed             | MN         |
| power_analysis.R               | R script to run power simulation                                      | MN         |
| powerplot_estimated.png        | Graph of power estimates per b3 value                                 | MN         |

### 03 Data
| File Name | Description| Created By |
|:--|:--|:--|
| 00extract_dates.R   | Helper function with regexes to extract dates                         | MN         |
| 00extract_pattern.R | Helper function to extract a pattern from text                        | MN         |
| 00html_to_txt_v2.R     | Helper function to convert html files to plain txt                    | MN         |
| 00scrape_html_dir.R | Helper function that allows scraping a directory of html files        | MN         |
| 01scrape_articles_v2.R | Function to run statcheck & scrape additional meta-data from articles | MN         |
| 02data_wrangling_v2.R  | Function to clean & reorder raw data                                  | MN         |
|2023-06-01scraped_articles_v2.txt|Raw statcheck data|MN|
|2023-06-15data_per_article_with_missings.txt|Data organized per article, including articles without NHST results|MN|
|2023-06-15data_per_article_with_stats.txt|Data organized per article, only including articles with NHST results|MN|
|2023-06-15data_wrangled_no_missings.txt|Clean data organized per NHST result, only including articles with NHST results|MN|
|2023-06-15data_wrangled_with_missings.txt|Clean data organized per NHST result, also including articles without NHST results (in these cases, only article level info is in the data frame)|MN|

### 04 Analysis

#### 01 Descriptives

| File Name | Description| Created By |
|:--|:--|:--|
|descriptives_V2.R|Main analysis file to create descriptive statistics, figures, and tables | MN |
|fig1_violin_plots.png|Figure 1|MN|
|fig2_line_graph_means.png|Figure 2|MN|
|table2.txt|Raw data from Table 2|MN|
|table3.txt|Raw data from Table 3|MN|

#### 02 Confirmatory

| File Name | Description| Created By |
|:--|:--|:--|
|confirmatory_analyses_v2.R|Main analysis script for confirmatory analyses|MN|
|lm_dec_errors.rda|R data object with output of the multilevel logistic model estimating the probability of a decision error|MN|
|lm_errors.rda|R data object with output of the multilevel logistic model estimating the probability of an error|MN|

#### 03 Exploratory

| File Name | Description| Created By |
|:--|:--|:--|
|bayes_factors.R|Calculating Bayes factors and posterior probabilities|MN|
|fig3_nr_nhst_over_time.png|Figure 3|MN|
|fig4_perc_articles_errors_over_time.png|Figure 4|MN|
|separate_journal_pairs.R|Fit regression models for journal pairs separately|MN|
|trends_over_time.R|Calculate and visualize trends over time|MN|

### renv

| File Name | Description| Created By |
|:--|:--|:--|
|renv (folder), .Rprofile, renv.lock|Files for syncing the same R package versions used in the data analysis|MN|
