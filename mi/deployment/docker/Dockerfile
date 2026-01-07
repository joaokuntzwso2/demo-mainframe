# ---------- 1) Build stage: create the CAR ----------
FROM maven:3.9-eclipse-temurin-17 AS builder
WORKDIR /build

COPY pom.xml .
COPY src src

# If you need extra files (connectors, configs) that affect the build, copy them too
# COPY deployment deployment

RUN mvn -q -DskipTests clean package

# ---------- 2) Runtime stage: WSO2 MI ----------
FROM wso2/wso2mi:4.4.0
ENV MI_HOME=/home/wso2carbon/wso2mi

COPY deployment/deployment.toml ${MI_HOME}/conf/deployment.toml
COPY deployment/docker/resources/ ${MI_HOME}/resources/
COPY deployment/libs/ ${MI_HOME}/dropins/
COPY --from=builder /build/target/*.car ${MI_HOME}/repository/deployment/server/carbonapps/

EXPOSE 8290 8253
