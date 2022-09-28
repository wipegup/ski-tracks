FROM openjdk:8-alpine

COPY target/default+uberjar/ski-tracks.jar /ski-tracks/app.jar

EXPOSE 8080

CMD ["java", "-jar", "/ski-tracks/app.jar"]
