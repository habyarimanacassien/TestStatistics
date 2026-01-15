# =============================================================================
#                          STATISTICAL TESTS PROGRAM                          #             
# =============================================================================

# Function to get alternative hypothesis
get_alternative <- function() {
  repeat {
    cat("\nChoose test type:\n")
    cat("1. Two-tailed\n")
    cat("2. Left-tailed\n")
    cat("3. Right-tailed\n")
    
    choice <- readline("Enter choice (1/2/3): ")
    
    if (choice == "1") return("two.sided")
    if (choice == "2") return("less")
    if (choice == "3") return("greater")
    
    cat("Invalid choice. Please try again.\n")
  }
}

# Function to get significance level (alpha)
get_alpha <- function() {
  repeat {
    cat("\n========== SIGNIFICANCE LEVEL (ALPHA) ==========\n")
    cat("1. alpha = 0.01\n")
    cat("2. alpha = 0.05\n")
    cat("3. alpha = 0.10\n")
    
    alpha_choice <- readline("Enter an option (1/2/3): ")
    
    if (alpha_choice %in% c("1", "2", "3")) break
    cat("Invalid option. Try again.\n")
  }
  
  alpha <- c(0.01, 0.05, 0.10)[as.numeric(alpha_choice)]
  cat("\nYou selected alpha =", alpha, "\n")
  
  return(alpha)
}

# Decision rule function
decision_rule <- function(p_value, alpha) {
  result <- if (p_value < alpha) {
    z <- paste0("Since p-value = ", round(p_value, 4), 
                " is less than alpha = ",alpha, ". Reject H0.")
  } else {
    z <- paste0("Since p-value = ", round(p_value, 4), 
                " is not less than alpha = ",alpha, ". Fail to reject H0.")
  }
  
  return(z)
}

# Function to get data input method
get_input_method <- function() {
  repeat {
    cat("\n========== DATA INPUT METHOD ==========\n")
    cat("1. Manual entry (type data)\n")
    cat("2. Import from CSV file\n")
    
    choice <- readline("Select method (1/2): ")
    
    if (choice %in% c("1", "2")) return(as.numeric(choice))
    cat("Invalid choice. Please try again.\n")
  }
}

# Helper function to select column by number
select_column <- function(data, prompt_text) {
  repeat {
    cat("\n", prompt_text, "\n", sep = "")
    col_names <- names(data)
    
    for (i in seq_along(col_names)) {
      cat(i, ". '", col_names[i], "'\n", sep = "")
    }
    
    choice <- readline("Select column data (1/2): ")
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(col_names)) {
      selected_col <- col_names[choice_num]
      cat("Selected column: '", selected_col, "'\n", sep = "")
      return(selected_col)
    }
    cat("Invalid choice. Please enter a digit 1-", length(col_names), "\n")
  }
}

# Helper function to read numeric vector manually
read_numeric_vector <- function(prompt_text) {
  cat(prompt_text)
  input <- readline()
  return(as.numeric(unlist(strsplit(input, "\\s+"))))
}

# Function to import data from CSV file
import_csv_data <- function(test_type) {
  repeat {
    file_path <- readline("Enter CSV file path: ")
    
    # Check if file exists
    if (!file.exists(file_path)) {
      cat("\nERROR: File not found. Please check the path and try again.\n")
      retry <- readline("Try again? (y/n): ")
      if (tolower(retry) != "y") return(NULL)
      next
    }
    
    # Try to read the CSV file
    tryCatch({
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      
      cat("\nFile loaded successfully!\n")
      cat("Columns found:", paste(names(data), collapse = ", "), "\n")
      cat("Number of rows:", nrow(data), "\n\n")
      
      # Validate and extract data based on test type
      if (test_type == "one_sample") {
        result <- extract_one_sample_data(data)
      } else if (test_type == "paired") {
        result <- extract_paired_data(data)
      } else if (test_type == "independent") {
        result <- extract_independent_data(data)
      } else if (test_type == "chisq") {
        result <- extract_chisq_data(data)
      }
      
      if (!is.null(result)) {
        return(result)
      } else {
        retry <- readline("\nTry another file? (y/n): ")
        if (tolower(retry) != "y") return(NULL)
      }
      
    }, error = function(e) {
      cat("\nERROR reading file:", e$message, "\n")
      retry <- readline("Try again? (y/n): ")
      if (tolower(retry) != "y") return(NULL)
    })
  }
}

# Extract data for one-sample t-test
extract_one_sample_data <- function(data) {
  col_name <- select_column(data, "Select column for sample data:")
  
  x <- as.numeric(data[[col_name]])
  x <- x[!is.na(x)]  # Remove NA values
  
  if (length(x) == 0) {
    cat("No valid numeric data found in the column.\n")
    return(NULL)
  }
  
  mu <- as.numeric(readline("Enter population mean (mu): "))
  
  cat("\nData extracted successfully!\n")
  cat("Sample size:", length(x), "\n")
  cat("Sample mean:", mean(x), "\n")
  cat("Sample SD:", sd(x), "\n")
  
  return(list(x = x, mu = mu))
}

# Extract data for paired t-test
extract_paired_data <- function(data) {
  col1 <- select_column(data, "Select first column:")
  col2 <- select_column(data, "Select second column:")
  
  x <- as.numeric(data[[col1]])
  y <- as.numeric(data[[col2]])
  
  # Remove rows with NA in either column
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  if (length(x) == 0 || length(y) == 0) {
    cat("No valid paired data found.\n")
    return(NULL)
  }
  
  cat("\nData extracted successfully!\n")
  cat("Number of pairs:", length(x), "\n")
  cat("Mean difference:", mean(x - y), "\n")
  
  return(list(x = x, y = y))
}

# Extract data for independent t-test
extract_independent_data <- function(data) {
  col1 <- select_column(data, "Select first sample column:")
  col2 <- select_column(data, "Select second sample column:")
  
  x <- as.numeric(data[[col1]])
  y <- as.numeric(data[[col2]])
  
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  if (length(x) == 0 || length(y) == 0) {
    cat("No valid data found in one or both columns.\n")
    return(NULL)
  }
  
  cat("\nData extracted successfully!\n")
  cat("Sample 1 size:", length(x), "| Mean:", mean(x), "\n")
  cat("Sample 2 size:", length(y), "| Mean:", mean(y), "\n")
  
  return(list(x = x, y = y))
}

# Extract data for chi-squared test
extract_chisq_data <- function(data) {
  cat("\nFor chi-squared test, the CSV should contain a contingency table.\n")
  cat("Rows represent categories of one variable, columns represent another.\n")
  cat("First column can be row labels (will be excluded from analysis).\n\n")
  
  # Ask if first column contains labels
  has_labels <- readline("Does the first column contain row labels? (y/n): ")
  
  if (tolower(has_labels) == "y") {
    observed <- as.matrix(data[, -1])  # Exclude first column
  } else {
    observed <- as.matrix(data)
  }
  
  # Convert to numeric and check
  observed <- apply(observed, 2, as.numeric)
  
  if (any(is.na(observed))) {
    cat("Non-numeric values found in the table.\n")
    return(NULL)
  }
  
  cat("\nContingency table extracted successfully!\n")
  cat("Dimensions:", nrow(observed), "rows x", ncol(observed), "columns\n")
  cat("\nExtracted table:\n")
  print(observed)
  
  return(list(observed = observed))
}

# =============================================================================
#                                 MAIN PROGRAM                                #
# =============================================================================

repeat {
  
  cat("\n========== STATISTICAL TEST MENU ==========\n")
  cat("1. One Sample t-test\n")
  cat("2. Paired t-test\n")
  cat("3. Independent t-test\n")
  cat("4. Chi-squared test\n")
  cat("5. Quit\n")
  
  choice <- readline("Select an option (1/2/3/4/5): ")
  
  # ---------------- ONE-SAMPLE T-TEST ----------------
  if (choice == "1") {
    cat("\n--- One Sample t-test ---\n")
    
    input_method <- get_input_method()
    
    if (input_method == 1) {
      # Manual entry
      x <- read_numeric_vector("Enter sample data (space-separated):  \n")
      mu <- as.numeric(readline("Enter population mean (mu): "))
    } else {
      # CSV import
      data_result <- import_csv_data("one_sample")
      if (is.null(data_result)) {
        cat("\nReturning to main menu.\n")
        next
      }
      x <- data_result$x
      mu <- data_result$mu
    }
    
    alpha <- get_alpha()
    alternative <- get_alternative()
    
    test <- t.test(x, mu = mu, alternative = alternative)
    
    print(test)
    decision <- decision_rule(test$p.value, alpha)
    cat("\nDecision:\n", decision, "\n")
  }
  
  # ---------------- PAIRED T-TEST ----------------
  else if (choice == "2") {
    cat("\n--- Paired t-test ---\n")
    
    input_method <- get_input_method()
    
    if (input_method == 1) {
      # Manual entry
      x <- read_numeric_vector("Enter first sample (x, space-separated): \n")
      y <- read_numeric_vector("Enter second sample (y, space-separated): \n")
      
      if (length(x) != length(y)) {
        cat("\nERROR:\n")
        cat("The paired t-test requires x and y to have the SAME length.\n")
        next
      }
    } else {
      # CSV import
      data_result <- import_csv_data("paired")
      if (is.null(data_result)) {
        cat("\nReturning to main menu.\n")
        next
      }
      x <- data_result$x
      y <- data_result$y
    }
    
    alpha <- get_alpha()
    alternative <- get_alternative()
    
    test <- t.test(x, y, paired = TRUE, alternative = alternative)
    
    print(test)
    decision <- decision_rule(test$p.value, alpha)
    cat("\nDecision:\n", decision, "\n")
  }
  
  # ---------------- INDEPENDENT T-TEST ----------------
  else if (choice == "3") {
    cat("\n--- Independent t-test ---\n")
    
    input_method <- get_input_method()
    
    if (input_method == 1) {
      # Manual entry
      x <- read_numeric_vector("Enter sample 1 (space-separated): \n")
      y <- read_numeric_vector("Enter sample 2 (space-separated): \n")
    } else {
      # CSV import
      data_result <- import_csv_data("independent")
      if (is.null(data_result)) {
        cat("\nReturning to main menu.\n")
        next
      }
      x <- data_result$x
      y <- data_result$y
    }
    
    alpha <- get_alpha()
    alternative <- get_alternative()
    
    test <- t.test(x, y, paired = FALSE, alternative = alternative)
    
    print(test)
    decision <- decision_rule(test$p.value, alpha)
    cat("\nDecision:\n", decision, "\n")
  }
  
  # ---------------- CHI-SQUARED TEST ----------------
  else if (choice == "4") {
    cat("\n--- Chi-squared Test of Independence ---\n")
    
    input_method <- get_input_method()
    
    if (input_method == 1) {
      # Manual entry
      rows <- as.integer(readline("Enter number of rows: \n"))
      cols <- as.integer(readline("Enter number of columns: \n"))
      
      cat("\nEnter the observed frequencies row by row:\n")
      
      observed <- matrix(0, nrow = rows, ncol = cols)
      
      for (i in 1:rows) {
        for (j in 1:cols) {
          prompt <- paste("Frequency at row", i, "column", j, ": ")
          observed[i, j] <- as.numeric(readline(prompt))
        }
      }
      
      cat("\nObserved Contingency Table:\n")
      print(observed)
    } else {
      # CSV import
      data_result <- import_csv_data("chisq")
      if (is.null(data_result)) {
        cat("\nReturning to main menu.\n")
        next
      }
      observed <- data_result$observed
    }
    
    alpha <- get_alpha()
    
    test <- chisq.test(observed)
    
    print(test)
    decision <- decision_rule(test$p.value, alpha)
    cat("\nDecision:\n", decision, "\n")
  }
  
  # ---------------- EXIT ----------------
  else if (choice == "5") {
    cat("\nExiting program. Thank you!\n")
    break
  }
  
  else {
    cat("\nInvalid selection. Please choose 1–5.\n")
  }
}