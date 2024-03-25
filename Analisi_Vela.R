library(readxl)
library(RColorBrewer)
library(comprehenr)

# Specify file path
file_path <- "C://Users//giova//Downloads//DATI VELA copia.xlsx"

# Read the Excel file
df <- read_excel(file_path)

sailing_classes = unique(df$`Sailing Class`)

# utils variables
sailing_class_dict <- c(
  "1" = "IQfoil men",
  "2" = "IQfoil women",
  "3" = "Kitefoil men",
  "4" = "Kitefoil women",
  "5" = "49er",
  "6" = "49er FX",
  "7" = "470 Mix",
  "8" = "ILCA 6",
  "9" = "ILCA 7",
  "10" = "Nacra17"
)

pathology_dict  <- c(
  "1" = "Concussion",
  "2" = "Fracture (traumatic)",
  "3" = "Stress fracture (overuse)",
  "4" = "Dislocation, subluxation",
  "5" = "Ligamentous rupture",
  "6" = "Sprain (joint and/or ligament)",
  "7" = "Lesion of meniscus or cartilage",
  "8" = "Strain/muscle injury",
  "9" = "Contusion/haematoma",
  "10" = "Tendinosis/tendinopathy",
  "11" = "Fasciitis/aponeurosis injury",
  "12" = "Spinal cord injury",
  "13" = "Muscle cramps",
  "14" = "Compartimental syndrome",
  "15" = "Acute overload"
)


myPalette = brewer.pal(n = 10, name = "Pastel1")
for (s in sailing_classes) {

  pathology_class_s <- df$Pathology[df$`Sailing Class` == s]
  idx = df$ID[df$`Sailing Class` == s]
  
  n = length(unique(idx))
  
  pathology_s_named <- to_vec(for(p in pathology_class_s) pathology_dict[p]) 
  values <- table(pathology_s_named)
  perc <- values / sum(values) * 100  # Calculate percentages
  
  # Create labels with both counts and percentages
  labels <- paste( names(values), "\n", round(perc, 1), "%", sep = "")
  
  
  name_file = paste("plot_", sailing_class_dict[s], ".pdf", sep = "")
  cat(paste(name_file, "\n"))
  pdf(name_file , width = 8, height = 7)
  
  #create main title 
  main <- paste("Sailing discipline: ",sailing_class_dict[[s]], "\n", "N. of athlete: ", n, "\n", "N. of injuries: ", sum(values), sep="")
  

  # Create a pie chart
  pie(values, 
      main = main, 
      labels = labels,
      col = myPalette)
  dev.off()
  
  
}




