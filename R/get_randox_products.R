get_randox_products <- function(){
	get_randox_product_urls() |>
	purrr::map(get_randox_product_details_from_url) |>
	purrr::keep(function(x){length(x$biomarkers) > 0})
}