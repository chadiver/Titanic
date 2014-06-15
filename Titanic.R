readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( paste(path.name, file.name, sep=""), 
            colClasses=column.types,
            na.strings=missing.types )
}

Titanic.path <- "~/Developer/Kaggle/Titanic/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('factor',    # Survived
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-1]
train.raw <- readData(Titanic.path, train.data.file,
                      train.column.types, missing.types)
df.train <- train.raw
test.raw <- readData(Titanic.path, test.data.file,
                     test.column.types, missing.types)
df.infer <- test.raw

require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map",
        col = c("yellow", "black"), legend = FALSE)
