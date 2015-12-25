files <- list.files(pattern=".out")
numFiles <- ceiling(sqrt(length(files)))

pdf("plots.pdf")
par(mfrow = c(numFiles,numFiles))

for (input in files) {
  data <- read.csv(input)
  
  id <- data[1];
  rtt <- data[2];
  
  frame <- data.frame(id, rtt);
  
  title <- sprintf("Per-Packet RTT [%s]", input);
  plot(frame, xlab="Packet ID", ylab="RTT (us)", main=title);
  
  print(input)
} 
