FROM openjdk:8-alpine

COPY target/uberjar/ski-tracks.jar /ski-tracks/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/ski-tracks/app.jar"]
