get_randox_product_details_from_url <- function(url){

  sampling_procedure <-
    dplyr::if_else(
      url |> stringr::str_detect("/in-clinic/"),
      "venous",
      "fingerprick"
    )


  remDr <- get_selenium_session()

  remDr$navigate(url)

  biomarkers_script <- "
    biomarkers = []

    biomarkers = biomarkers.concat(Array.from(document.querySelectorAll('.what-we-testtest-list li')).map((x) => x.textContent))

    Array.from(document.getElementsByClassName('what-we-testexpand')).forEach(function(y){
      y.click()
      biomarkers = biomarkers.concat(Array.from(document.querySelectorAll('.what-we-testtest-list li')).map((x) => x.textContent))
    })

    return(biomarkers)
    "

  biomarkers <- remDr$executeScript(biomarkers_script) |> unlist() |>
    stringr::str_to_lower() |> 
    stringr::str_replace_all("[^a-z0-9]+","-") |> 
    stringr::str_replace_all("(^\\-|\\-$)","")

  product_html <- remDr$getPageSource() |> dplyr::first() |> xml2::read_html()

  title <- product_html |>
    rvest::html_nodes(".product-page-heading") |>
    rvest::html_text2()    

  title <- paste0(title, dplyr::if_else(
      sampling_procedure == "venous",
      " (in-clinic)",
      " (at home)"
    ))

  price_pence <- product_html |>
   rvest::html_nodes(".product-page-price") |>
   rvest::html_text2() |>
   stringr::str_remove_all("(From )?[\\.£]") |> 
   as.integer() |> max()

  biomarkers_map <- readr::read_csv("data-raw/biomarker_snomed_map.csv", col_types="cc")

  biomarkers <- tibble::tibble(biomarker_handle = biomarkers) |>
    dplyr::left_join(biomarkers_map,  by="biomarker_handle") |>
    dplyr::pull("sctid") |>
    na.omit() |>
    unique()

  list(
    name = jsonlite::unbox(title),
    url = jsonlite::unbox(url),
    biomarkers = biomarkers,
    sampling_procedure = jsonlite::unbox(sampling_procedure),
    price_pence = jsonlite::unbox(price_pence)
  )

}
