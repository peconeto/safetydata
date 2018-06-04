# safetydata

This is a R library to help aviation safety data analysts perform common tasks. Install with:

```{r}
devtools::install_github("peconeto/safetydata", build_vignettes = TRUE)
```
To learn more about the library and its contents, after installation:

```{r}
browseVignettes("safetydata")
```

Note that certain functions and features depend on GE Aviation's Rems package. To install it:
```{r}
devtools::install_github("ge-flight-analytics/Rems")
```
