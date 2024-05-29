# Load all required packages
# Load tidyverse package for data analysis
library(tidyverse)
# Load readxl package for reading excel file
library(readxl)
# Load rsample package for modeling
library(rsample)
# Load naniar package for imputation
library(naniar)
# Load gridExtra package for combining plots
library(gridExtra)
# Load gbm package for Logistic regression modeling
library(gbm)
# Load randomForest package for Logistic regression modeling
library(randomForest)
# Load pROC package for modeling
library(pROC)
# Load rpart package for Random Forest modeling
library(rpart)
# Load ROCR package
library(ROCR)
# Load DT package for table display
library(DT)
# Load the leaflet package for map charts
library(leaflet)
# Set default theme
theme_set(theme_minimal())

# read_files
telco_data <- read_excel("Telco_customer_churn_data.xlsx")
# Display the first 500 rows of imported data
datatable(
  telco_data[1:500,],
  filter = "top",
  caption = "The first 500 rows of loaded data.",
  options = list(pageLength = 50,
                 scrollY = "500px",
                 scrollX = TRUE))


# data_str
# Visualize missing data
gg_miss_var(telco_data) + theme(text = element_text(size = 14),
                                axis.text.x = element_text(size = 12),
                                axis.text.y = element_text(size = 12))
# Check for class of each variable
class_table <- sapply(telco_data, class)
class_table <- data.frame(
  Variable = names(class_table),
  Class = as.character(class_table),
  stringsAsFactors = FALSE)
# Check for the proportion of missing values in full data
x <- miss_var_summary(telco_data)
# Rename the columns 
colnames(x) <- c("Variable", "Values_missing", "Proportion_missing")
# Round the Proportion_missing column to 2 decimal points
x$Proportion_missing <- round(x$Proportion_missing, 2)
# Combine data frames on the "Variable" column
properties <- merge(class_table, x, by = "Variable")
# Display the properties of the data
datatable(
  properties,
  caption = "Table displaying the properties of the data.")

# clean_data
# Step 1: Drop non-required variables in data
telco_cleaned <- telco_data %>%
  select(
    -CustomerID, # Does not provide any insights to churning
    -Count, # Same value no variation
    -Country, # Same value no variation
    -State, # Same value no variation
    - City, # Duplicate as Zip Code could serve same purpose
    -`Lat Long`, # Duplicate that includes Latitude and Longitude
    -`Churn Value`, # Replicate of Churn Label
    -`Churn Reason` # High proportion of missing values
  )
# Step 2: Rename variables using underscore instead of space
names(telco_cleaned) <- c("Zip_Code", "Latitude",
                          "Longitude", "Gender", "Senior_Citizen",
                          "Partner", "Dependents", "Tenure_Months",
                          "Phone_Service", "Multiple_Lines",
                          "Internet_Service", "Online_Security",
                          "Online_Backup", "Device_Protection",
                          "Tech_Support", "Streaming_TV",
                          "Streaming_Movies", "Contract",
                          "Paperless_Billing", "Payment_Method",
                          "Monthly_Charges", "Total_Charges", 
                          "Churn_Label", "Churn_Score", "CLTV")
# Step 3: Impute missing values for Total_Charges variable with the mean value 
telco_cleaned$Total_Charges <- impute_mean(telco_cleaned$Total_Charges)
# Step 4: Convert all categorical variables from character to factor
# Group all character variables
cat_variables <- c("Gender", "Senior_Citizen", "Partner", "Dependents",
                   "Phone_Service", "Multiple_Lines", "Internet_Service",
                   "Online_Security", "Online_Backup", "Device_Protection",
                   "Tech_Support", "Streaming_TV", "Streaming_Movies", 
                   "Contract", "Paperless_Billing", "Payment_Method",
                   "Churn_Label")
# Convert from character variables to factor variables
telco_cleaned[cat_variables] <- lapply(telco_cleaned[cat_variables], 
                                       as.factor)

# Step 5: Encode binary variables from Yes/No to 1/0
# Group all binary variables
bin_variable <- c("Senior_Citizen", "Partner", "Dependents",
                  "Phone_Service", "Multiple_Lines", "Online_Security",
                  "Online_Backup", "Device_Protection", "Tech_Support",
                  "Streaming_TV", "Streaming_Movies", "Paperless_Billing",
                  "Churn_Label")
# Convert from binary variables to numeric values
telco_cleaned[bin_variable] <- lapply(telco_cleaned[bin_variable],
                                      function(z) ifelse(z == "Yes", 1, 0))
# Display the first 500 rows of the cleaned and reformatted data
datatable(
  telco_cleaned[1:500,],
  caption = "First 500 rows of cleaned data with new created variables.",
  options = list(pageLength = 50,
                 scrollY = "500px",
                 scrollX = TRUE))
# Churn Map

telco_data_grouped <- telco_data %>%
    group_by(`Churn Label`) %>%
    mutate(`Churn Label` = as.factor(`Churn Label`))

# Create two separate groups for churned and non-churned customers
non_churned_markers <- telco_data_grouped %>%
    filter(`Churn Label` == "No")

churned_markers <- telco_data_grouped %>%
    filter(`Churn Label` == "Yes")

# Plotting data on Leaflet map
leaflet() %>%
    addTiles() %>%
  # Add first markers for non-churned customers
    addCircleMarkers(data = non_churned_markers,
                     ~Longitude, ~Latitude,
                     radius = 3,
                     color = "blue",
                     fillOpacity = 0.2,
                     stroke = FALSE,
                     popup = ~paste("City:", City,
                                    "<br>Zip Code:", `Zip Code`,
                                    "<br>Gender:", Gender,
                                    "<br>Senior Citizen:", `Senior Citizen`,
                                    "<br>Partner:", Partner,
                                    "<br>Dependents:", Dependents,
                                    "<br>Tenure Months:", `Tenure Months`),
                     label = ~CustomerID,
                     # Group non-churned markers
                     group = "Not Churned") %>%
  # Add second markers for churned customers
    addCircleMarkers(data = churned_markers, 
                     ~Longitude, ~Latitude,
                     radius = 3,
                     color = "red",
                     fillOpacity = 0.2,
                     stroke = FALSE,
                     popup = ~paste("City:", City,
                                    "<br>Zip Code:", `Zip Code`,
                                    "<br>Gender:", Gender,
                                    "<br>Senior Citizen:", `Senior Citizen`,
                                    "<br>Partner:", Partner,
                                    "<br>Dependents:", Dependents,
                                    "<br>Tenure Months:", `Tenure Months`),
                     label = ~CustomerID,
                     # Group churned markers
                     group = "Churned") %>%
  # Add layers control for interactive legend
    addLayersControl(overlayGroups = c("Churned", "Not Churned"),
                     position = "bottomleft",
                     options = layersControlOptions(
                       collapsed = FALSE)) %>% # Expand the control
  # Add Legend to the bottomleft of the map
    addLegend("bottomleft",
              colors = c("red", "blue"), # Legend colors
              labels = c("Churned", "Not Churned"), # Legend labels
              title = "Churn Label")

# tenure_dist
ggplot(telco_cleaned, aes(x = Tenure_Months)) +
    geom_histogram(binwidth = 1,
                   fill = "blue",
                   color = "black") +
    labs(title = "Tenure Distribution of Customers",
         x = "Tenure in Months", y = "Number of Customers") +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18))

# contract
ggplot(telco_cleaned, aes(x = Contract, fill = Contract)) +
    geom_bar() +
    labs(title = "Customer Contract Preference of Customers",
         x = "Contract Type", y = "Number of Customers") +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18))

# payment
ggplot(telco_cleaned, aes(x = Payment_Method, fill = Payment_Method)) +
    geom_bar() +
    labs(title = "Method of Payment Preferred by Customers",
         x = "Payment Method", y = "Number of Customers") +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18))


# avg_contract
telco_cleaned %>%
    group_by(Contract) %>%
    summarise(Avg_Monthly_Charge = round(mean(Monthly_Charges),2),
              Avg_Total_Charge = round(mean(Total_Charges),2)) %>%
    datatable()

# avg_payment
telco_cleaned %>%
    group_by(Payment_Method) %>%
    summarise(Avg_Monthly_Charge = round(mean(Monthly_Charges),2),
              Avg_Total_Charge = round(mean(Total_Charges),2)) %>%
    datatable()

# churn_analysis1}
# Churn Distribution
churn_rate <- telco_cleaned %>%
    count(Churn_Label) %>%
    mutate(Customer_proportion = round(n / sum(n) * 100, 0))
# Plot churn distribution
ggplot(churn_rate, aes(x = factor(Churn_Label, 
                                  labels = c("Didn't Churn", "Churned")), 
                       y = n, 
                       fill = factor(Churn_Label))) +
    geom_col() +
    labs(title = "Churn Distribution for all Customers",
         x = "Churn Label",
         y = "Number of Customers") +
    geom_text(aes(label = paste0(round(Customer_proportion, 0), "%")),
              position = position_stack(vjust = 0.5),
              color = "black", size = 6) +
    scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                      labels = c("Didn't Churn", "Churned"),
                      guide = FALSE)  +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))

# churn_analysis2
# Create a subset of data for customers who churned
churned_customers <- telco_cleaned %>%
    filter(Churn_Label == 1)
# Analyze Contract Type by churn
d1 <- ggplot(churned_customers, aes(x = Contract, fill = Contract)) +
        geom_bar() +
        labs(title = "Contract Type of Churned Customers", 
             x = "Contract", y = "Number of Customers") +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
# Analyze Tenure Months by churn
d2 <- ggplot(churned_customers, aes(x = Tenure_Months)) +
        geom_histogram(binwidth = 1,
                       fill = "blue",
                       color = "black") +
        labs(title = "Tenure of Churned Customers",
             x = "Tenure in Months", y = "Number of Customers") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
# Analyze Internet Service Type by churn
d3 <- ggplot(churned_customers,
             aes(x = Internet_Service, fill = Internet_Service)) +
        geom_bar() +
        labs(title = "Internet Service of Churned Customers", 
             x = "Internet Service", y = "Number of Customers") +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
# Analyze Payment Method Type by churn
d4 <- ggplot(churned_customers,
       aes(x = Payment_Method, fill = Payment_Method)) +
    geom_bar() +
    labs(title = "Payment Method of Churned Customers", 
         x = "Payment Method", y = "Number of Customers") +
    scale_x_discrete(labels=c("Bank Transfer\n(automatic)", 
                                "Credit Card\n(automatic)",
                                "Electronic\nCheck",
                               "Mailed\nCheck")) +
    theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18)) 
# Print charts
grid.arrange(d1, d2, d3, d4, ncol = 2)

# churn_analysis3
# Calculate Churn Rate per Gender
# Calculate total number of customers for each gender
total_customers_gender <- telco_cleaned %>%
  group_by(Gender) %>%
  summarize(total_customers = n())

# Calculate Churn Rate for all customers
churn_rate_all <- telco_cleaned %>%
  group_by(Gender, Churn_Label) %>%
  count() %>%
  left_join(total_customers_gender, by = "Gender") %>%
  mutate(Customer_proportion = n / total_customers * 100)

# Plot Churn Rate for all customers
s1 <- ggplot(churn_rate_all,
       aes(x = factor(Churn_Label, 
                      labels = c("Didn't Churn", "Churned")), 
           y = Customer_proportion, 
           fill = factor(Churn_Label))) +
  geom_col() +
  # Facet chart by Gender
  facet_wrap(~Gender) +
  geom_text(aes(label = paste0(round(Customer_proportion, 0), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black", size = 6) +
  labs(title = "Churn Rate of Customers by Gender",
       x = "Churn Label", y = "Percentage of Customers") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("Didn't Churn", "Churned"),
                    guide = FALSE)  +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18),
              # increase facet title size
              strip.text = element_text(size = 16))

# Calculate Churn Rate for Senior Citizens
# Filter the dataset to include only senior customers
churned_seniors <- telco_cleaned[telco_cleaned$Senior_Citizen == 1,]
# Calculate Churn Rate of Senior Citizens
churn_rate_senior <- churned_seniors %>%
    group_by(Churn_Label) %>%
    summarize(Customer_proportion = n() / nrow(churned_seniors) * 100)
# Plot Churn Rate of Senior Citizen
s2 <- ggplot(churn_rate_senior,
             aes(x = factor(Churn_Label, 
                            labels = c("Didn't Churn", "Churned")), 
                 y = Customer_proportion, 
                 fill = factor(Churn_Label))) +
        geom_col() +
        labs(title = "Churn Rate of Senior Citizens",
             x = "Churn Label", y = "Proportion of Customers") +
          geom_text(aes(label = paste0(round(Customer_proportion, 0), 
                                       "%")),
            position = position_stack(vjust = 0.5), 
            color = "black", size = 6) +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("Didn't Churn", "Churned"),
                    guide = FALSE)  +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))

# Calculate Churn Rate for those with Dependents
# Filter the dataset to include only those with dependents
churned_dependents <- telco_cleaned[telco_cleaned$Dependents == 1,]
# Calculate Churn Rate of those with dependents
churn_rate_dependents <- churned_dependents %>%
    group_by(Churn_Label) %>%
    summarize(Churn_Rate = n() / nrow(churned_dependents) * 100)
# Plot Churn Rate of those with dependents
s3 <- ggplot(churn_rate_dependents,
             aes(x = factor(Churn_Label, 
                            labels = c("Didn't Churn", "Churned")), 
                 y = Churn_Rate, 
                 fill = factor(Churn_Label))) +
        geom_col() +
        labs(title = "Churn Rate of of those with Dependents",
             x = "Churn Label", y = "Percentage of Customers") +
          geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black", size = 6) +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("Didn't Churn", "Churned"),
                    guide = FALSE)  +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))

# Calculate Churn Rate for those with a Partner
# Filter the dataset to include only those with a partner
churned_partner <- telco_cleaned[telco_cleaned$Partner == 1,]
# Calculate Churn Rate of those with a partner
churn_rate_partner <- churned_partner %>%
    group_by(Churn_Label) %>%
    summarize(Churn_Rate = n() / nrow(churned_partner) * 100)
# Plot Churn Rate of those with a partner
s4 <- ggplot(churn_rate_partner,
             aes(x = factor(Churn_Label, 
                            labels = c("Didn't Churn", "Churned")), 
                 y = Churn_Rate, 
                 fill = factor(Churn_Label))) +
        geom_col() +
        labs(title = "Churn Rate of of those with a Partner",
             x = "Churn Label", y = "Percentage of Customers") +
          geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black", size = 6) +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("Didn't Churn", "Churned"),
                    guide = FALSE)  +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
# Print charts
grid.arrange(s1, s2, s3, s4, ncol = 2)

# churn_analysis4
churn_rate_by_Phone_Service <- telco_cleaned %>%
  group_by(Phone_Service) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Multiple_Lines <- telco_cleaned %>%
  group_by(Multiple_Lines) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Online_Security <- telco_cleaned %>%
  group_by(Online_Security) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Online_Backup <- telco_cleaned %>%
  group_by(Online_Backup) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Device_Protection <- telco_cleaned %>%
  group_by(Device_Protection) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Tech_Support <- telco_cleaned %>%
  group_by(Tech_Support) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Streaming_TV <- telco_cleaned %>%
  group_by(Streaming_TV) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Streaming_Movies <- telco_cleaned %>%
  group_by(Streaming_Movies) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
churn_rate_by_Paperless_Billing <- telco_cleaned %>%
  group_by(Paperless_Billing) %>%
  summarize(Churn_Rate = mean(Churn_Label) * 100)
# Create a bar plot to visualize churn rates of those with phone service
p1 <- ggplot(churn_rate_by_Phone_Service, 
             aes(x = as.factor(Phone_Service), y = Churn_Rate, 
                 fill = as.factor(Phone_Service))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Phone Service", 
       x = "Use Phone Service", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of those with multiple lines
p2 <- ggplot(churn_rate_by_Multiple_Lines, 
             aes(x = as.factor(Multiple_Lines), y = Churn_Rate, 
                 fill = as.factor(Multiple_Lines))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Multiple Lines", 
       x = "Use  Multiple_Lines", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of those with online security
p3 <- ggplot(churn_rate_by_Online_Security, 
             aes(x = as.factor(Online_Security), y = Churn_Rate, 
                 fill = as.factor(Online_Security))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Online Security", 
       x = "Use Online Security", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of those with online backup
p4 <- ggplot(churn_rate_by_Online_Backup, 
             aes(x = as.factor(Online_Backup), y = Churn_Rate, 
                 fill = as.factor(Online_Backup))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Online Backup", 
       x = "Use Online Backup", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of those with device protection
p5 <- ggplot(churn_rate_by_Device_Protection, 
             aes(x = as.factor(Device_Protection), y = Churn_Rate, 
                 fill = as.factor(Device_Protection))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Device Protection", 
       x = "Use Device Protection", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of those with tech support
p6 <- ggplot(churn_rate_by_Tech_Support, 
             aes(x = as.factor(Tech_Support), y = Churn_Rate, 
                 fill = as.factor(Tech_Support))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Tech Support", 
       x = "Use tech support", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of that stream TV
p7 <- ggplot(churn_rate_by_Streaming_TV, 
             aes(x = as.factor(Streaming_TV), y = Churn_Rate, 
                 fill = as.factor(Streaming_TV))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Stream TV", 
       x = "Get to Stream TV", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of that stream Movies
p8 <- ggplot(churn_rate_by_Streaming_Movies, 
             aes(x = as.factor(Streaming_Movies), y = Churn_Rate, 
                 fill = as.factor(Streaming_Movies))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Stream Movies", 
       x = "Get to Stream Movies", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Create a bar plot to visualize churn rates of with Paperless billing
p9 <- ggplot(churn_rate_by_Paperless_Billing, 
             aes(x = as.factor(Paperless_Billing), y = Churn_Rate, 
                 fill = as.factor(Paperless_Billing))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Churn_Rate, 0), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  labs(title = "Paperless Billing", 
       x = "Get Paperless Billing", 
       y = "Churn Rate") +    
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))
# Print charts
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)

# churn_analysis5
# Set seed for reproducibility
set.seed(123)
# Split data on Churn Label into 70% for training, 30% for testing
data_split <- initial_split(telco_cleaned, prop = 0.7, strata = Churn_Label)
train_data <- training(data_split)
test_data <- testing(data_split)
# Build a logistic regression model for probabilities
churn_model <- glm(Churn_Label ~ .,
                   data = train_data,
                   family = "binomial")
# Predict churn probabilities on the test set
churn_probabilities <- predict(churn_model, 
                               newdata = test_data, 
                               type = "response")
# Evaluate model performance using ROC curve
roc <- roc(test_data$Churn_Label, churn_probabilities)
plot(roc, main = "ROC Curve")
# Set threshold value
threshold <- 0.5
# Convert churn probabilities to binary predictions (0 or 1) based on threshold value set above
predicted_churn <- ifelse(churn_probabilities >= threshold, 1, 0)
# Compare predicted churn with actual churn in the test set
confusion_matrix <- table(predicted_churn, test_data$Churn_Label)
# Calculate accuracy, precision, and recall using confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# Precision = True Positives / (True Positives + False Positives)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
# Recall = True Positives / (True Positives + False Negatives)
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])

# churn_classification
# Set seed for reproducibility
set.seed(123)
# Build a Decision Tree model for classification
churn_tree <- rpart(Churn_Label ~ ., 
                    data = train_data, 
                    method = "class")
# Predict probabilities on the test set
churn_probabilities <- predict(churn_tree, 
                               newdata = test_data, 
                               type = "prob")[, 2]
# Create a prediction object
pred <- prediction(churn_probabilities, test_data$Churn_Label)
# Calculate gain chart
gain <- performance(pred, "tpr", "fpr")
# Plot gain chart
plot(gain, main = "Gain Chart for Decision Tree Model")
# Evaluate model performance using a confusion matrix
tree_confusion_matrix <- table(churn_probabilities, test_data$Churn_Label)
# Calculate accuracy = True Positives / (All Observations)
accuracy2 <- sum(diag(tree_confusion_matrix)) / sum(tree_confusion_matrix)
# Calculate Precision = True Positives / (True Positives + False Positives)
precision2 <- tree_confusion_matrix[2, 2] / sum(tree_confusion_matrix[, 2])
# Calculate Recall = True Positives / (True Positives + False Negatives)
recall2 <- tree_confusion_matrix[2, 2] / sum(tree_confusion_matrix[2, ])  