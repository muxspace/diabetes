FROM zatonovo/r-base

COPY . /app/diabetes
WORKDIR /app/diabetes
RUN crant -SCi
