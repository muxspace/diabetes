FROM zatonovo/r-base

RUN rpackage roxygen2 randomForest

COPY . /app/diabetes
WORKDIR /app/diabetes
RUN crant -SCi
