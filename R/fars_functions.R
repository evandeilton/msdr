#####################################################
########## Assignement 2: Documenting Code ##########
#####################################################

# what each function does, in general terms;
# the function arguments (inputs);
# each function's return value;
# conditions that may result in an error;
# functions that need to be imported from external packages;
# examples of each function's usage

#' Reads data from a file
#' 
#' This is a simple function to read data from .csv extension and return a tible in R.
#' 
#' @param filename text string containing the full path and name of the file to be read.
#' 
#' @return a tible, table, data.frame data object
#' 
#' @details this function read data using the function \code{\link{read_csv}} from package \code{readr}. This function is capable to read \code{csv} files compressed in .bz2 format. The next step is convert the data into a tible format by using the function \code{\link{tbl_df}} from package \code{dplyr}. If the full path is not well passed the function may crash.
#' 
#' @importFrom "readr" read_csv
#' @importFrom "dplyr" tbl_df
#' 
#' @examples 
#' \dontrun{
#' path <- "accident_2013.csv.bz2"
#' x <- fars_read(path)
#' class(x)
#' head(x)
#' }
#' 
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create correct path to read data from a file
#' 
#' This is a simple function to prepare a full correct path to the function \code{\link{fars_read}}.
#' 
#' @param year text string containing value of the year in format "2013" being text or number.
#' 
#' @return a string describing the full correct path to the file to be read.
#' 
#' @details the value from this function intends to be used to the function \code{\link{fars_read}} since it require a well formated full path from the file to be read. If the file to be read is not in the correct directory the string will not be good to the function \code{\link{fars_read}}.
#' 
#' @note make sure that the file to read is in the main directory before run this function along with \code{\link{fars_read}}.
#' 
#' @examples 
#' \dontrun{
#' path <- "2013"
#' x <- make_filename(path)
#' class(x)
#' head(x)
#' head(fars_read(x))
#' }
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Reads data from several files
#' 
#' This function to read several .csv extension files and returns a list of tidy files containing only Month and Year inside of each.
#' 
#' @param years vector or list of values repsenting years: eg c(2013, 2014) or list(2013, 2014).
#' 
#' @return A lis of a tible objects
#' 
#' @details This function along with \code{\link{make_filename}}, \code{\link{fars_read}} reads data from .csv types only receiving as param a vector or list of years by the user and returns a list of tibles containing variables YEAR and MONTH present inside those data. The function \code{\link{mutate}} and \code{\link{select}} from package \code{dplyr} are used to tidy data.
#' 
#' @note the tidy process require the pipe operator from package \code{tidyr}. If this package is not pre-loaded the function returns an error.
#'  
#' @importFrom "dplyr" mutate select
#' 
#' @examples 
#' \dontrun{
#' year <- c(2013, 2014)
#' x <- fars_read_years(year)
#' lapply(x, function(x) head(x, 2))
#' }
#' 
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Reads data from several files and returns the row number of each one
#' 
#' This function to read several .csv extension files and returns a tible where each column represents a year and each row represents a month. The values column vs row are the row number of each dataset/year.
#' 
#' @param years vector or list of values repsenting years: eg c(2013, 2014) or list(2013, 2014).
#' 
#' @return A tible containg the rown umbers of each dataset by month and year.
#' 
#' @details This function along with \code{\link{fars_read_years}} reads data from .csv types only receiving as param a vector or list of years by the user and returns a tible containing the row number by month and year where each row is a month and eacho column is a year passed by the user and present in the data sets. This function invokes the functions \code{\link{bind_rows}}, \code{\link{group_by}} and \code{\link{summarize}} from package \code{dplyr} and \code{\link{spread}} from \code{tidyr}.
#' 
#' @note the tidy process require the pipe operator from package \code{tidyr}. If this package is not pre-loaded the function returns an error.
#'  
#' @importFrom "tidyr" spread
#' @importFrom "dplyr" bind_rows group_by summarize
#' 
#' @examples 
#' \dontrun{
#' years <- c(2013, 2014, 2015)
#' x <- fars_summarize_years(years)
#' class(x)
#' head(x)
#' }
#' 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Reads .csv data and plots a map of states
#' 
#' This function to read a .csv extension file and returns a plot of the State map. The user must pass the state number and a year of data fot that.
#' 
#' @param state.num a single value of US state number. eg: 1, 10 or 12;
#' @param year a single value representing a year. eg. 2013, 2014
#' 
#' @return a plot of a US State.
#' 
#' @details This function along with \code{\link{make_filename}} and \code{\link{fars_read}} reads data from .csv types only receiving as param a value of state number and year and plot a map of the input State. This function invokes the functions \code{\link{filter}} from package \code{dplyr} and \code{\link{map}} from \code{maps} also as \code{\link{points}} from \code{graphics}.
#' 
#' @note if any of those packages where not instaled or the arguments passed are wrong no plot ir shown.
#'
#' @importFrom "maps" map
#' @importFrom "dplyr" filter
#' @importFrom "graphics" points
#' 
#' @examples 
#' \dontrun{
#' state.num <- 10; year <- 2013
#' fars_map_state(state.num, year)
#' }
#' 
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}


################# DATA ##########################


#' Accidents in US National Highway Traffic 2013
#'
#' @source This is data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes in 2013.
#' @format A .csv compressed in .bz2 format to read with \code{\link{read_csv}} from package \code{readr}. 
#' \describe{
#' 50 variables and 30202 observations
#' }
#' @examples
#' \dontrun{
#'  accident_2013
#' }
"accident_2013"

#' Accidents in US National Highway Traffic 2014
#'
#' @source This is data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes in 2014.
#' @format A .csv compressed in .bz2 format to read with \code{\link{read_csv}} from package \code{readr}. 
#' \describe{
#' 50 variables and 30056 observations
#' }
#' @examples
#' \dontrun{
#'  accident_2014
#' }
"accident_2014"


#' Accidents in US National Highway Traffic 2015
#'
#' @source This is data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes in 2015.
#' @format A .csv compressed in .bz2 format to read with \code{\link{read_csv}} from package \code{readr}. 
#' \describe{
#' 50 variables and 32166 observations
#' }
#' @examples
#' \dontrun{
#'  accident_2015
#' }
"accident_2015"


