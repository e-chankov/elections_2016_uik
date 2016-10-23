require(readr)
require(data.table)

roundfixS <- function(x){
  x<- x/sum(x)*10000
  n <- length(x)
  x0 <- floor(x)
  e <- x - x0
  S. <- sum(e)
  stopifnot(all.equal(S., (S <- round(S.))))
  if (S > 0) {
    r <- numeric(n)
    r[sort.list(e, decreasing = TRUE)[1:S]] <- 1
    x <- x0 + r
  }
  x/100
}

parties.titles <- c("rodina", "communists",
                    "pensioners", "edinaya_rossia",
                    "greens", "civil_platform",
                    "LDPR", "PARNAS",
                    "ROST", "civil_power",
                    "yabloko", "CPRF",
                    "patriots", "sprav_rossia")
names(parties.titles) <- c("Родина", "Коммунисты России",
                           "Партия пенсионеров",
                           "Единая Россия","Зеленые",
                           "Гражданская платформа",
                           "ЛДПР", "ПАРНАС",
                           "Партия роста", "Гражданская сила", 
                           "Яблоко", "КПРФ",
                           "Патриоты России",
                           "Справедливая Россия")


uik.data <- read_tsv("data/table_233_level_4.txt")
persentage.results <- apply(uik.data[,c(13, 23:36)], 1, roundfixS)
issued.ballots <- rowSums(uik.data[,7:9])
turnout <- round(issued.ballots/uik.data[,5] * 100, 2) 
uik.results <- data.table(uik.data[,23:36], t(persentage.results)[,-1], turnout, 
                                         uik.data[,1:4])
rm(uik.data, persentage.results, turnout, issued.ballots)
setnames(uik.results, 1:14, parties.titles)
setnames(uik.results, 15:28, paste0(parties.titles, ".pct"))
setnames(uik.results, 29, "turnout")
choices <- c("turnout", paste0(parties.titles,'.pct'))
names(choices) <- c("Явка", names(parties.titles))
regions <- c("Российская Федерация", unique(uik.results$regions))
save(uik.results, choices, regions, file = "uik.data.Rdata")
