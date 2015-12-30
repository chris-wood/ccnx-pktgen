files <- list.files(pattern=".out")
numFiles <- ceiling(sqrt(length(files)))

#pdf("plots.pdf");
#par(mfrow = c(1, 1))

for (input in files) {
  tryCatch({
    data <- read.csv(input)
    
    id <- data[1];
    rtt <- data[2];
    
    frame <- data.frame(id, rtt);
  
    pdf(paste("plot-", input, ".pdf", sep = ""));
    title <- sprintf("Per-Packet RTT [%s]", input);
    plot(frame, xlab="Packet ID", ylab="RTT (us)", main=title);
    dev.off()
  
    #print(input);
    
  }, warning = function(w) {
    # pass
  }, error = function(e) {
    # pass
  }, finally = {
    # pass
  });
} 

