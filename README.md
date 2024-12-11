# ProntoPCR <img src="/images/dottori_lab_pentagon.svg" alt="Logo" align="right" width="180">

**ProntoPCR** is a user-friendly and efficient R Shiny application designed for real-time PCR (qPCR) or reverse transcriptase qPCR (RT-qPCR) data analysis. It automates calculations such as averaging housekeeper genes, ΔCq, ΔΔCq, 2^-ΔCq, 2^-ΔΔCq, making it easy to analyse gene expression data.

## How It Works

1. **Upload Data**: Import your qPCR data (Cq means, Target, Samples) in CSV format.
2. **Set Parameters**: Select housekeeping genes and/or control groups + target genes.
3. **Analyse**: ProntoPCR calculates key metrics automatically. Users can download results as a CSV file.
4. **Statistics**: Select and perform desired statistics on calculated data. Users can download statistic results as a HTML file.
5. **Visualise**: Customise and download publication-ready graphs directly from the app.

## Availability

ProntoPCR has been designed to operate both online and locally. Whilst we aim to maintain online availability, the hosted platform may change or become unavailable. Therefore, it is recommended to rely on the local version, which functions with the same features as the online option. The local version also does not require internet access once installed. To run the application locally, the user needs to download [R](https://cran.r-project.org/) and [RStudio](https://posit.co/downloads/).

### Local Installation

For detailed installation instructions, especially for users unfamilar with the R coding language, refer to the [ProntoPCR Handbook](). 

To use **ProntoPCR**, you can install it directly from GitHub within the R console:

```r
# Install devtools package if not already installed
install.packages("devtools")

# Load devtools library within R session
library(devtools)

# Install ProntoPCR from GitHub
devtools::install_github("MarnieMaddock/ProntoPCR")

# Run the application
library(ProntoPCR)
ProntoPCR()
```
### Online Access
[ProntoPCR Online](https://marniem.shinyapps.io/ProntoPCR/)

## Instuctions for Use
How to use instructions are available within the [ProntoPCR Handbook]().

## Citation
If ProntoPCR is used for your analysis, please cite the journal article:


## Feedback and Support
If you encounter any issues or have suggestions, feel free to:

- Open an issue on this repository
- [Email Us](mlm715@uowmail.edu.au)

## Contributions
We welcome contributions. If you'd like to contribute, please:

- Fork this repository.
- Create a new branch for your feature or bug fix.
- Submit a pull request with a detailed explanation of your changes.
  
## License
ProntoPCR is licensed under the MIT License. See [LICENSE](https://github.com/MarnieMaddock/ProntoPCR/blob/main/LICENSE) for details.

---- 

![Footer](/images/footer.svg)
