get_randox_product_urls <- function(){
  list_urls <- c("https://randoxhealth.com/en-GB/in-clinic-health-test", "https://randoxhealth.com/en-GB/home-health-test")

  list_urls |> purrr::map(function(list_url){
    get_html_for_url(list_url) |>
      rvest::html_nodes("a.package-btn") |> 
      rvest::html_attr("href") |>
      rvest::url_absolute(list_url)
  }) |> unlist() |> unique()

}