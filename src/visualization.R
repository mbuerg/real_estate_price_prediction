# visualization

data_complete_imputated <- read.csv(path_data_complete_imputated)

ggplot(data=data_complete_imputated, aes(x=SalePrice, y=after_stat(density))) +
  geom_histogram(color="black", fill="blue") +
  geom_density(color = "orange", fill = "orange", alpha = 0.6) +
  labs(x = "Immobilienpreis", 
        y = "Dichte", 
        title = paste("Verteilung von Immobilienpreisen in den USA"),
        caption = "Quelle: R, ggplot2") +
  apatheme
