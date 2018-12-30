#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Your Name - Data Driven Securty                          #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping
# install.packages("xml2")
# install.packages("httr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggplot2")
 library("xml2")
 library("httr")
 library("tidyr")
 library("dplyr")
 library("ggplot2")
 library("scales")

 ### 1.1 Obtención de la página web
url <- "https://www.mediawiki.org/wiki/MediaWiki"
htmlpage <- xml2::read_html(url)

### 1.2 Analisis de el contenido de la web  --------------------------------------------------------------------------------
title <- xml2::xml_text(xml2::xml_find_all(htmlpage,"//title"))

### 1.3.	Extracción de enlaces  -----------------------------------------------------------------------------------------
linksContent <- xml2::xml_text(xml2::xml_find_all(htmlpage,"//a"))
linksUrl <- xml2::xml_attr(xml2::xml_find_all(htmlpage,"//a"), "href")
links <- data.frame(Link_Name = linksContent, Link_url = linksUrl,stringsAsFactors = FALSE) %>% tidyr::drop_na()

### 1.4 Exploración de enlaces -----------------------------------------------------------------------------------------
results <- data.frame(Link = character(), status_code = integer(), stringsAsFactors = FALSE)
# Delete same page navigation ( eg. #resources)
outterLinks <- dplyr::filter(links, !startsWith(links[,2],"#"))

for (rownum in 1:nrow(outterLinks)) {
  linkToTest <- outterLinks[rownum,"Link_url"]
  if (!grepl("^http",linkToTest)) {
   linkToTest <- paste("https://www.mediawiki.org",linkToTest,sep = "")

  }
  message <- paste("Sending a request to ", linkToTest, sep = "")
  print(message)
  r <- httr::HEAD(linkToTest)
  results <- rbind(results,data.frame(Link = linkToTest, status_code = r$status_code, stringsAsFactors = FALSE))
  message <- paste("With status code: ", r$status_code, sep = "")
  print(message)
  Sys.sleep(1)
}

### Gráficos en R  -----------------------------------------------------------------------------------------

### 2.1 Histograma  -----------------------------------------------------------------------------------------


### 2.2 Un gráfico de barras  -----------------------------------------------------------------------------------------
links <- cbind(links,data.frame(is_outter = (grepl("^http",links[,"Link_url"]) & !grepl("https:[/][/]www[.]mediawiki[.]org",links[,"Link_url"]))))
ggplot2::ggplot(links, ggplot2::aes(is_outter)) + ggplot2::geom_bar() + ggplot2::scale_x_discrete("Other domains")
### 2.3 Pie Chart  -----------------------------------------------------------------------------------------
results_resumee <- as.data.frame(results %>% count(status_code))
results_resumee[,'status_code'] <- factor(results_resumee[,'status_code']) #convert to factor value
ggplot2::ggplot(results_resumee, aes(x = "", y = n ,fill = status_code)) + geom_bar(width = 1,stat = "identity") + coord_polar("y", start = 0) + geom_text(aes(y = n/2 + c(0, cumsum(n)[-length(n)]), label = percent(n/sum(n))), size=5)

