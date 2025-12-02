DDEA learning course in reproducible R programming

# AdvancedR3:

This project is to learn how to structure R projects to make them
collaborative and reproducible over time

# Brief description of folder and file contents

The following folders contain:

-   `data/`: Formatted data
-   `data-raw/`: Raw data
-   `docs/`: Developing code
-   `R/`: Tested code

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("pak")
pak::pak()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
