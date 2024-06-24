library(httr)
library(jsonlite)

#' Search for Administrative Districts in South Korea
#'
#' This function retrieves information about administrative districts in South Korea
#' using the Vworld API. It can fetch data for different levels of administrative divisions:
#' provinces (sido), cities/counties (sigungu), townships (dongmyun), and villages (ri).
#' The function performs client-side filtering based on the parent_code parameter.
#'
#' @param api_key Character string. The Vworld API key.
#' @param district_type Character string. The type of district to search for.
#'   Must be one of 'sido', 'sigungu', 'dongmyun', or 'ri'.
#' @param parent_code Character string, optional. The code of the parent district
#'   to filter results. For 'sigungu', use the 2-digit sido code (e.g., "11" for Seoul).
#'   For 'dongmyun', use the 5-digit sigungu code. For 'ri', use the 8-digit dongmyun code.
#' @param debug Logical, default FALSE. If TRUE, prints debug information during the API call.
#'
#' @return A data frame with two columns:
#'   \item{code}{The district code}
#'   \item{name}{The district name in Korean}
#'
#' @details
#' The function retrieves all districts of the specified type from the Vworld API,
#' then filters the results based on the parent_code parameter. This allows for
#' precise filtering of administrative districts.
#'
#' The filtering is performed as follows:
#' - For 'sigungu' search: Returns districts where the first 2 digits of the code match the parent_code.
#' - For 'dongmyun' search: Returns districts where the first 5 digits of the code match the parent_code.
#' - For 'ri' search: Returns districts where the first 8 digits of the code match the parent_code.
#'
#' Note that this function may retrieve a large amount of data from the API before
#' filtering, which could impact performance for large datasets.
#'
#' @examples
#' \dontrun{
#' # Set your API key
#' api_key <- Sys.getenv("VWORLD_API_KEY")
#'
#' # Search for all provinces (sido)
#' sido_results <- search_adm_districts(api_key, district_type = "sido")
#'
#' # Search for all cities/counties (sigungu) in Seoul (code starts with "11")
#' seoul_sigungu <- search_adm_districts(api_key, district_type = "sigungu", parent_code = "11")
#'
#' # Search for all townships (dongmyun) in Jongno-gu, Seoul (code starts with "11110")
#' jongno_dongmyun <- search_adm_districts(api_key, district_type = "dongmyun", parent_code = "11110")
#'
#' # Search for all villages (ri) in a specific dongmyun (using 8-digit dongmyun code)
#' specific_ri <- search_adm_districts(api_key, district_type = "ri", parent_code = "11110110")
#' }
#'
#' @export
#' @importFrom httr GET content modify_url
#' @importFrom jsonlite fromJSON toJSON

library(httr)
library(jsonlite)

search_adm_districts <- function(api_key, district_type, parent_code = NULL, debug = FALSE) {
  if (missing(api_key) || is.null(api_key) || api_key == "") {
    stop("API key is missing or empty. Please provide a valid Vworld API key.")
  }

  base_url <- "https://api.vworld.kr/req/data"
  params <- list(
    key = api_key,
    domain = "https://r2bit.com/BitStat2",
    service = "data",
    version = "2.0",
    request = "getfeature",
    format = "json",
    size = 1000,
    page = 1,
    geometry = "false",
    attribute = "true",
    crs = "EPSG:3857",
    geomfilter = "BOX(13663271.680031825,3894007.9689600193,14817776.555251127,4688953.0631258525)"
  )

  if (district_type == "sido") {
    params$data <- "LT_C_ADSIDO_INFO"
    code_col <- "ctprvn_cd"
    name_col <- "ctp_kor_nm"
  } else if (district_type == "sigungu") {
    params$data <- "LT_C_ADSIGG_INFO"
    code_col <- "sig_cd"
    name_col <- "sig_kor_nm"
    if (!is.null(parent_code)) params$attrfilter <- paste0("sig_cd:like:", parent_code, "%")
  } else if (district_type == "dongmyun") {
    params$data <- "LT_C_ADEMD_INFO"
    code_col <- "emd_cd"
    name_col <- "emd_kor_nm"
    if (!is.null(parent_code)) params$attrfilter <- paste0("emd_cd:like:", parent_code, "%")
  } else if (district_type == "ri") {
    params$data <- "LT_C_ADRI_INFO"
    code_col <- "li_cd"
    name_col <- "li_kor_nm"
    if (!is.null(parent_code)) params$attrfilter <- paste0("li_cd:like:", parent_code, "%")
  } else {
    stop("Invalid district_type. Must be one of 'sido', 'sigungu', 'dongmyun', or 'ri'.")
  }

  if (debug) print(paste("Request URL:", modify_url(base_url, query = params)))

  res <- GET(base_url, query = params)

  if (debug) {
    print(paste("Status code:", status_code(res)))
    print(paste("Headers:", toJSON(headers(res), auto_unbox = TRUE)))
  }

  if (status_code(res) != 200) {
    stop(paste("API Error:", content(res)$error$text))
  }

  content_text <- content(res, "text", encoding = "UTF-8")
  if (debug) print(paste("Response content:", substr(content_text, 1, 1000), "..."))

  data <- fromJSON(content_text)

  if (debug) print(paste("Response structure:", str(data)))

  if (is.null(data$response$result$featureCollection$features) || length(data$response$result$featureCollection$features) == 0) {
    warning("No features found in the response. Check your parameters and API key.")
    return(data.frame(code = character(0), name = character(0)))
  }

  features <- data$response$result$featureCollection$features

  result <- data.frame(
    code = features$properties[[code_col]],
    name = features$properties[[name_col]]
  )

  # Additional client-side filtering if needed
  if (!is.null(parent_code) && nrow(result) > 0) {
    if (district_type == "sigungu") {
      result <- result[substr(result$code, 1, nchar(parent_code)) == parent_code, ]
    } else if (district_type == "dongmyun" || district_type == "ri") {
      result <- result[substr(result$code, 1, nchar(parent_code)) == parent_code, ]
    }
  }

  if (nrow(result) == 0) {
    warning(paste("No results found for", district_type, "with parent_code", parent_code))
  }

  return(result)
}
