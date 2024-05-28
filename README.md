# Green Book Vegetation Monitoring App

This Shiny app provides a user-friendly interface for monitoring vegetation data. The app is designed to help analyze and visualize various ecological datasets.

## How to Run the App Locally

1. Clone the repository:
    ```sh
    git clone https://github.com/inyo-gov/inyoShiny.git
    cd inyoShiny
    ```

2. Open the project in RStudio:
    - Double-click on the `inyoShiny.Rproj` file to open the project in RStudio. This will ensure all project settings and working directories are correctly configured.

3. Install the required libraries in R:
    ```r
    install.packages(c("shiny", "tidyverse", "here", "crosstalk", "plotly", "janitor", "sf", "leaflet", "DT"))
    ```

4. Run the app in RStudio:
    - In the RStudio Console, run:
    ```r
    shiny::runApp()
    ```

## Reproducing and Testing the App

To reproduce and test the app:

1. Ensure you have cloned the repository and opened the `.Rproj` file in RStudio as described above.

2. Load the required libraries by running the following in the RStudio Console:
    ```r
    install.packages(c("shiny", "tidyverse", "here", "crosstalk", "plotly", "janitor", "sf", "leaflet", "DT"))
    ```

3. Source the functions and data by running:
    ```r
    source("code/functions.R")
    ```

4. Run the app by executing:
    ```r
    shiny::runApp()
    ```

5. To test specific functions or datasets, you can create separate R scripts within the RStudio project, load the necessary data, and call the functions directly.

## Contributing

To contribute to this project, you can:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch`).
3. Make your changes.
4. Commit your changes (`git commit -am 'Add new feature'`).
5. Push to the branch (`git push origin feature-branch`).
6. Create a new Pull Request.

## Issues

If you encounter any issues or have questions, feel free to open an issue on the [GitHub repository](https://github.com/inyo-gov/inyoShiny/issues).

## Deployment

The app is deployed and accessible at [https://inyo.shinyapps.io/inyoShiny/](https://inyo.shinyapps.io/inyoShiny/).
