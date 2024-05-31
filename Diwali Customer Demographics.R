tuesdata <- tidytuesdayR::tt_load('2023-11-14')
dat <- tuesdata$diwali_sales_data

#clean data
dat$Gender <- as.factor(dat$Gender)
dat$Marital_Status <- as.factor(dat$Marital_Status)
levels(dat$Marital_Status) <- c("Married", "Single")

dat$Product_Category <- gsub("Household items", "Household Items",
                                      dat$Product_Category)
unique(dat$Product_Category)

#look at categories
unique(dat$Product_Category)

#observe what categories sell the most
barchart(dat$Product_Category)


Sales <- ggplot(data = dat, 
                aes(x = Orders, y = fct_rev(fct_infreq(Product_Category)),
                    fill = Gender)) +
  geom_col() +
  scale_fill_manual(values = c("F" = "#891E1B",
                               "M" = "#F79E1F")) +
  labs(title = "Sales by Gender",
       x = "Orders",
       y = "Product Category") +
  theme_classic() + 
  ggsave("Diwali.png", width = 10, height = 5)

Job <- ggplot(dat,
              aes(x = Orders, y = fct_rev(fct_infreq(Occupation)),
                  fill = Product_Category)) +
  geom_col() + 
  scale_fill_manual(values = c(
    "Auto" = "#531B1F", "Beauty" = "#AB2722", "Books" = "#891E1B",
    "Clothing & Apparel" = "#F79E1F", "Decor" = "#704021",
    "Electronics & Gadgets" = "#C85549", "Food" = "#E27F32",
    "Footwear & Shoes" = "#EDBB4D", "Furniture" = "#F5D58A",
    "Games & Toys" = "#B63277", "Hand & Power Tools" = "brown",
    "Household Items" = "#751F83", "Office" = "#842C5C",
    "Pet Care" = "lavender", "Sports Products" = "#A75502",
    "Stationery" = "#FFFDD0", "Tupperware" = "beige",
    "Veterinary" = "tan")) +
  labs(title = "Customer Purchases by Occupation",
       x = "Orders",
       y = "Occupation") +
  theme_classic()


#Fix legend format to remove _
Job <- Job + guides(fill = guide_legend(title = "Product Category"))

Job

age_group <- ggplot(dat,
                    aes(x = Orders, y = `Age Group`,
                        fill = Marital_Status)) +
  geom_col() +
  scale_fill_manual(values = c("Married" = "#B63277",
                               "Single" = "#751F83")) +
  labs(title = "Sales by Age Group") +
  theme_classic()

age_group <- age_group + guides(fill = guide_legend(title = "Age Group"))

#combine plots together
big_plot <- (Sales + age_group)/Job +
  plot_annotation(title = "Diwali Customer Demographics")
big_plot  
